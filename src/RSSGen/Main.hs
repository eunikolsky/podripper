{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module RSSGen.Main
  ( rssGenParser
  , run
  ) where

import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.List (intercalate, sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Ord (Down(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version (Version, showVersion)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Network.HTTP.Simple
import Options.Applicative
import qualified System.Environment as Env

import qualified Paths_ripper as Paths (version)
import RSSGen.Database
import RSSGen.Downloader
import RSSGen.RSSFeed
import RSSGen.RSSItem
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
          processUpstreamRSS (T.pack podcastTitle) config conn
          generateFeed config conn out
          liftIO $ closeDatabase conn
        Nothing -> fail
          $ "Couldn't parse feed config files "
          <> intercalate ", " feedConfigFiles

-- | Generates the feed at the requested path.
generateFeed :: RSSFeedConfig -> Connection -> FilePattern -> Action ()
generateFeed feedConfig conn out = do
  -- we need the audio files to generate the RSS, which are in the
  -- directory of the same name as the podcast title
  let podcastTitle = dropExtension out
  mp3Files <- getDirectoryFiles "" [podcastTitle </> "*.mp3"]
  let findUpstreamItem = closestUpstreamItemToTime
        (closestUpstreamItemInterval feedConfig)
        (T.pack podcastTitle)
        conn
  rssItems <- liftIO . fmap (newestFirst . catMaybes) $ traverse (rssItemFromFile podcastTitle findUpstreamItem) mp3Files

  version <- askOracle $ RSSGenVersion ()
  writeFile' out $ feed (ProgramVersion . showVersion $ version) feedConfig rssItems

newestFirst :: [RSSItem] -> [RSSItem]
newestFirst = sortOn Down

-- | Downloads the upstream RSS from the URL in the config, parses it and
-- saves the items in the database.
processUpstreamRSS :: MonadIO m => UpstreamRSSFeed.PodcastId -> RSSFeedConfig -> Connection -> m ()
processUpstreamRSS podcastTitle config conn = void $ runMaybeT $ do
  url <- MaybeT . pure $ upstreamRSSURL config
  text <- MaybeT . liftIO . downloadRSS conn . T.unpack $ url
  items <- MaybeT . pure . eitherToMaybe . UpstreamRSSFeed.parse podcastTitle $ text
  -- if we're here, then we have items
  liftIO $ saveUpstreamRSSItems conn items

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

downloadRSS :: (MonadIO m, MonadThrow m) => Connection -> URL -> m (Maybe T.Text)
downloadRSS conn = fmap (fmap TE.decodeUtf8) . getFile httpBS conn
