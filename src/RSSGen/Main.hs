{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module RSSGen.Main
  ( rssGenParser
  , run
  ) where

import Control.Monad.Logger.CallStack
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.List (intercalate, sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Ord (Down(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock
import Data.Version (Version, showVersion)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Options.Applicative
import qualified System.Environment as Env

import qualified Paths_ripper as Paths (version)
import RSSGen.Database
import RSSGen.Downloader
import RSSGen.Duration
import RSSGen.MonadTime
import RSSGen.PollHTTP
import RSSGen.RSSFeed
import RSSGen.RSSItem
import RSSGen.RunUntil
import qualified RSSGen.UpstreamRSSFeed as UpstreamRSSFeed

newtype RSSGenVersion = RSSGenVersion ()
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult RSSGenVersion = Version

-- | CLI options parser to get the RSS filenames (if any) that should be built.
-- This replaces `shake`'s built-in parser as it can't be used now since this
-- RSS generator is one of the commands in the executable. This parser only
-- parses target filenames, so all `shake` options are hardcoded.
rssGenParser :: Parser (NonEmpty FilePath)
rssGenParser = fmap (fromMaybe handleNothing . NE.nonEmpty) . some $ strArgument
  ( help "RSS filenames to build"
  <> metavar "RSS_FILE..."
  )

  where handleNothing = error "Impossible: empty list from `some`"

run :: NonEmpty FilePath -> IO ()
run filenames = do
  shakeDir <- fromMaybe "/var/lib/podripper/shake" <$> Env.lookupEnv "SHAKE_DIR"

  -- `shake` doesn't parse any CLI options itself, unlike `shakeArgs`
  -- see also: https://stackoverflow.com/questions/51355993/how-to-extend-shake-with-additional-command-line-arguments/51355994#51355994
  -- TODO this function doesn't print the final status message "Build completed in Xs"
  -- even though `shakeArgs` did that by default
  shake shakeOptions { shakeFiles = shakeDir, shakeColor = True } $ do
    want $ NE.toList filenames

    getRSSGenVersion <- addOracle $ \RSSGenVersion{} -> return Paths.version

    versioned 24 $ "*.rss" %> \out -> do
      void . getRSSGenVersion $ RSSGenVersion ()

      configDir <- getEnvWithDefault "/usr/share/podripper" "CONF_DIR"
      let podcastTitle = dropExtension out
      (feedConfig, feedConfigFiles) <- liftIO $ parseFeedConfig configDir podcastTitle
      need feedConfigFiles
      case feedConfig of
        Just config -> do
          -- note: `openDatabase` and `closeDatabase` are not exception-safe
          -- here, but it's fine since the program will exit soon anyway;
          -- I have tried refactoring this to `withDatabase`, and ultimately
          -- failed because sqlite library's `withConnection` uses `IO`, whereas
          -- the functions here have to be in `Action` :( (`MonadUnliftIO` may
          -- possibly help, but I doubt that)
          conn <- liftIO $ openDatabase DefaultFile

          let podcastId = T.pack podcastTitle
          ripFiles <- getRipFilesNewestFirst podcastId
          -- generate the feed with what we have immediately without possibly
          -- waiting for the upstream feed updates
          generateFeed config conn out podcastId ripFiles

          let maybeNewestRipTime = ripTime <$> listToMaybe ripFiles
          pollUpstreamRSSIfPossible podcastId config conn maybeNewestRipTime
          -- generate the feed again after possibly getting some upstream feed
          -- updates
          generateFeed config conn out podcastId ripFiles

          liftIO $ closeDatabase conn
        Nothing -> fail
          $ "Couldn't parse feed config files "
          <> intercalate ", " feedConfigFiles

-- | Returns a list of `RipFile`s for the given podcast, sorted from newest to
-- oldest.
getRipFilesNewestFirst :: UpstreamRSSFeed.PodcastId -> Action [RipFile]
getRipFilesNewestFirst podcastTitle = do
  -- we need the audio files to generate the RSS, which are in the
  -- directory of the same name as the podcast title
  mp3Files <- getDirectoryFiles "" [T.unpack podcastTitle </> "*.mp3"]
  liftIO $ newestFirst . catMaybes <$> traverse ripFileFromFile mp3Files

-- | Generates the feed at the requested path.
generateFeed :: RSSFeedConfig -> DBConnection -> FilePattern -> UpstreamRSSFeed.PodcastId -> [RipFile] -> Action ()
generateFeed feedConfig conn out podcastTitle ripFiles = do
  let findUpstreamItemIfPossible = maybe
        -- if there is no `upstreamFeedConfig` for the podcast, the returning
        -- function will always return `Nothing`
        (const $ pure Nothing)
        (\conf -> findUpstreamItem podcastTitle conf conn)
        (upstreamFeedConfig feedConfig)
  rssItems <- liftIO $ traverse (rssItemFromRipFile podcastTitle findUpstreamItemIfPossible) ripFiles
  version <- fmap (ProgramVersion . showVersion) . askOracle $ RSSGenVersion ()
  writeFileChanged out $ feed version feedConfig rssItems

findUpstreamItem :: UpstreamRSSFeed.PodcastId -> UpstreamFeedConfig -> DBConnection -> UTCTime -> IO (Maybe UpstreamRSSFeed.UpstreamRSSItem)
findUpstreamItem podcastTitle upstreamFeedConfig = closestUpstreamItemToTime
  (closestUpstreamItemInterval upstreamFeedConfig)
  podcastTitle

newestFirst :: [RipFile] -> [RipFile]
newestFirst = sortOn Down

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

-- | Runs `pollUpstreamRSS` if the (newest) `RipTime` is available, that is if
-- there are any files for the given podcast and the upstream feed is set.
pollUpstreamRSSIfPossible :: MonadIO m
  => UpstreamRSSFeed.PodcastId -> RSSFeedConfig -> DBConnection -> Maybe RipTime -> m ()
pollUpstreamRSSIfPossible podcastId config conn maybeNewestRipTime = void . runMaybeT $
  poll <$> MaybeT (pure maybeNewestRipTime) <*> MaybeT (pure $ upstreamFeedConfig config)

  where
    poll ripTime upstreamConfig@UpstreamFeedConfig{pollingRetryDelay, pollingDuration} = do
      now <- getCurrentTime
      let pollingEndTime = addUTCTime (toNominalDiffTime pollingDuration) now

      liftIO . runStderrLoggingT $
        pollUpstreamRSS podcastId upstreamConfig pollingRetryDelay pollingEndTime conn ripTime

-- | Waits until we have an upstream RSS item for the given (newest) rip. First,
-- it waits until the upstream RSS changes (which is typically within a few
-- hours after the live stream), but it doesn't mean that it now contains the
-- latest episode; so then it still needs to verify there is an upstream item
-- for the given rip, and wait for changes again if that fails.
pollUpstreamRSS :: (MonadTime m, MonadThrow m, MonadLogger m, MonadIO m)
  -- TODO too many parameters?
  => UpstreamRSSFeed.PodcastId -> UpstreamFeedConfig -> RetryDelay -> UTCTime -> DBConnection -> RipTime -> m ()
pollUpstreamRSS podcastTitle upstreamFeedConfig retryDelay endTime conn ripTime =
  void $ runUntil "pollRSS" retryDelay endTime iter

  where
    checkExistsUpstreamItemForRipTime = liftIO $ isJust <$> findUpstreamItem podcastTitle upstreamFeedConfig conn (utcTime ripTime)

    -- | Polls for upstream RSS changes, parses the RSS if any and saves the
    -- items in the database.
    processUpstreamRSS = void . runMaybeT $ do
      -- I had to remove the `MonadTrans` wrapper from `runUntil` (and
      -- thus pollHTTP`), which would allow the repeated action not to
      -- require unnecessary constraints (e.g. `MonadTime`), but
      -- surprisingly it doesn't seem to be a big issue (yet?); having the
      -- `MonadTrans` return type caused this type error:
      --
      -- • Couldn't match type ‘t0 m0’ with ‘IO’
      -- Expected: IO (Maybe Bytes)
      -- Actual: t0 m0 (Maybe Bytes)
      let url = T.unpack $ upstreamRSSURL upstreamFeedConfig
      bytes <- MaybeT $ pollHTTP retryDelay endTime conn url
      let text = TE.decodeUtf8 bytes
      allItems <- MaybeT . pure . eitherToMaybe $ UpstreamRSSFeed.parse podcastTitle text
      let items = limitItems upstreamFeedConfig allItems
      -- if we're here, then we have items
      liftIO $ saveUpstreamRSSItems conn items

    iter = do
      existsUpstreamItemForRipTime <- checkExistsUpstreamItemForRipTime
      if existsUpstreamItemForRipTime
        then pure $ Result ()
        else do
          processUpstreamRSS

          -- since the polling above may take a while, we want to check whether
          -- it has produced the upstream item that we need right after
          -- receiving new RSS (if any), without waiting for another
          -- `retryDelay` before another loop
          existsUpstreamItemAfterPolling <- checkExistsUpstreamItemForRipTime
          pure $ if existsUpstreamItemAfterPolling
            then Result ()
            else NoResult

-- | Limits the upstream feed items based on the `maxItems` setting.
-- Assumption: the RSS file contains newest to oldest items.
limitItems :: UpstreamFeedConfig -> [UpstreamRSSFeed.UpstreamRSSItem] -> [UpstreamRSSFeed.UpstreamRSSItem]
limitItems conf items = maybe items (`take` items) $ maxItems conf
