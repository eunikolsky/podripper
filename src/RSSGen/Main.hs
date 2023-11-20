{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module RSSGen.Main
  ( rssGenParser
  , run
  ) where

import Control.Monad.IO.Unlift
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
import GHC.Generics
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

-- | This oracle is needed to politely ask `shake` to poll for upstream RSS
-- changes, which is basically a wrapper around `pollUpstreamRSSIfPossible`
-- (using it directly does not work!). The question's types here are the
-- parameters types of the function (`DBConnection` can't be persisted
-- (`Binary`), so it can't be a parameter) and the answer's type is the
-- function's return type. The main effect of the oracle is to update the
-- database with the latest upstream RSS items.
--
-- A nice side effect of using the oracle is that even though we're asking for
-- the upstream item every time the RSS is generated, it won't be updated if
-- the item doesn't change (there are two layers of caching here though: one is
-- the database for all items and the other is `shake` for this particular
-- item).
newtype UpstreamItem = UpstreamItem (UpstreamRSSFeed.PodcastId, FilePath, RSSFeedConfig, Maybe UTCRipTime)
  deriving newtype (Show, Typeable, Eq, Hashable, Binary, NFData)
  deriving (Generic)
type instance RuleResult UpstreamItem = Maybe UpstreamRSSFeed.UpstreamRSSItem

newtype RSSGenVersion = RSSGenVersion ()
  deriving newtype (Show, Typeable, Eq, Hashable, Binary, NFData)
  deriving (Generic)
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
  let shakeOpts = shakeOptions
        { shakeFiles = shakeDir
        , shakeColor = True
        , shakeVerbosity = Diagnostic
        , shakeLint = Just LintBasic
        }

  -- `shake` doesn't parse any CLI options itself, unlike `shakeArgs`
  -- see also: https://stackoverflow.com/questions/51355993/how-to-extend-shake-with-additional-command-line-arguments/51355994#51355994
  -- TODO this function doesn't print the final status message "Build completed in Xs"
  -- even though `shakeArgs` did that by default
  shake shakeOpts $ do
    want $ NE.toList filenames

    void . addOracle $ \RSSGenVersion{} -> return Paths.version

    pollRSSItem <- addOracle $ \(UpstreamItem (podcastId, relPath, feedConfig, maybeRipTime)) ->
      pollUpstreamRSSIfPossible podcastId relPath feedConfig maybeRipTime

    -- note: this pattern supports relative paths to `.rss` now to be able to
    -- generate the RSS from the podripper's root data directory (parent of
    -- `complete/`); however generating the same RSS from `complete/` and from
    -- its parent seems to rebuild it every time
    versioned 28 $ "//*.rss" %> \out -> do
      configDir <- getEnvWithDefault "/usr/share/podripper" "CONF_DIR"
      let podcastTitle = takeBaseName out
          relPath = takeDirectory out
      (feedConfig, feedConfigFiles) <- liftIO $ parseFeedConfig configDir podcastTitle
      need feedConfigFiles
      case feedConfig of
        Just config ->
          actionWithDatabase (DefaultFile relPath) $ \conn -> do
            let podcastId = T.pack podcastTitle
            ripFiles <- getRipFilesNewestFirst relPath podcastId
            -- generate the feed with what we have immediately without possibly
            -- waiting for the upstream feed updates
            generateFeed config conn out podcastId ripFiles

            let maybeNewestRipTime = utcRipTime . ripTime <$> listToMaybe ripFiles
            -- it may seem strange to ignore the result of the oracle, but the
            -- main effect is to update the database because we'll need all
            -- items for all found rips; the result value is used by `shake` to
            -- determine whether to generate the feed
            void . pollRSSItem $ UpstreamItem (podcastId, relPath, config, maybeNewestRipTime)
            -- generate the feed again after possibly getting some upstream feed
            -- updates
            generateFeed config conn out podcastId ripFiles

        Nothing -> fail
          $ "Couldn't parse feed config files "
          <> intercalate ", " feedConfigFiles

-- | Allows to define an `Action` that has an exception-safe access to the
-- database.
--
-- Note: `Database.withDatabase` can't be used here because sqlite library's
-- connection functions use `IO`, whereas the function here has to be in
-- `Action` :( (`MonadUnliftIO` may possibly help, but I doubt that).
-- `actionBracket` is a special case defined in `shake`.
actionWithDatabase :: FileSpec -> (DBConnection -> Action a) -> Action a
actionWithDatabase f = actionBracket (openDatabase f) closeDatabase

-- | Returns a list of `RipFile`s for the given podcast, sorted from newest to
-- oldest. `relPath` is the path to the `complete/` directory relative to the
-- current directory; it's expected to be `complete` or `.`.
getRipFilesNewestFirst :: FilePath -> UpstreamRSSFeed.PodcastId -> Action [RipFile]
getRipFilesNewestFirst relPath podcastTitle = do
  -- we need the audio files to generate the RSS, which are in the
  -- directory of the same name as the podcast title
  mp3Files <- getDirectoryFiles relPath [T.unpack podcastTitle </> "*.mp3"]
  liftIO $ newestFirst . catMaybes <$> traverse (ripFileFromFile relPath) mp3Files

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

-- | Runs `pollUpstreamRSS` if the (newest) `UTCRipTime` is available, that is if
-- there are any files for the given podcast and the upstream feed is set.
pollUpstreamRSSIfPossible :: MonadIO m
  => UpstreamRSSFeed.PodcastId -> FilePath -> RSSFeedConfig -> Maybe UTCRipTime -> m (Maybe UpstreamRSSFeed.UpstreamRSSItem)
pollUpstreamRSSIfPossible podcastId relPath RSSFeedConfig{upstreamFeedConfig} maybeNewestRipTime =
  case (maybeNewestRipTime, upstreamFeedConfig) of
    (Just ripTime, Just config) -> liftIO $ poll ripTime config
    _ -> pure Nothing

  where
    poll ripTime upstreamConfig@UpstreamFeedConfig{pollingRetryDelay, pollingDuration} = do
      now <- getCurrentTime
      let pollingEndTime = addUTCTime (toNominalDiffTime pollingDuration) now

      withDatabase (DefaultFile relPath) $ \conn -> runStderrLoggingT $
        pollUpstreamRSS podcastId upstreamConfig pollingRetryDelay pollingEndTime conn ripTime

-- | Waits until we have an upstream RSS item for the given (newest) rip. First,
-- it waits until the upstream RSS changes (which is typically within a few
-- hours after the live stream), but it doesn't mean that it now contains the
-- latest episode; so then it still needs to verify there is an upstream item
-- for the given rip, and wait for changes again if that fails.
pollUpstreamRSS :: (MonadTime m, MonadThrow m, MonadLogger m, MonadUnliftIO m)
  -- TODO too many parameters?
  => UpstreamRSSFeed.PodcastId -> UpstreamFeedConfig -> RetryDelay -> UTCTime -> DBConnection -> UTCRipTime -> m (Maybe UpstreamRSSFeed.UpstreamRSSItem)
pollUpstreamRSS podcastTitle upstreamFeedConfig retryDelay endTime conn ripTime =
  fromStepResult <$> runUntil "pollRSS" retryDelay endTime iter

  where
    checkUpstreamItemForRipTime = liftIO $ findUpstreamItem podcastTitle upstreamFeedConfig conn (toUTCTime ripTime)

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
      maybeUpstreamItemForRipTime <- checkUpstreamItemForRipTime
      case maybeUpstreamItemForRipTime of
        Just item -> pure $ Result item
        Nothing -> do
          processUpstreamRSS

          -- since the polling above may take a while, we want to check whether
          -- it has produced the upstream item that we need right after
          -- receiving new RSS (if any), without waiting for another
          -- `retryDelay` before another loop
          toStepResult <$> checkUpstreamItemForRipTime

-- | Limits the upstream feed items based on the `maxItems` setting.
-- Assumption: the RSS file contains newest to oldest items.
limitItems :: UpstreamFeedConfig -> [UpstreamRSSFeed.UpstreamRSSItem] -> [UpstreamRSSFeed.UpstreamRSSItem]
limitItems conf items = maybe items (`take` items) $ maxItems conf
