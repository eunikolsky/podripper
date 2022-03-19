{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.List
import Data.Maybe
import Data.Ord (Down(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Version (Version, showVersion)
import Database.SQLite.Simple
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified System.Environment as Env

import qualified Paths_rssgen as Paths (version)
import Database
import Downloader
import RSSFeed
import RSSItem
import qualified UpstreamRSSFeed

newtype RSSGenVersion = RSSGenVersion ()
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult RSSGenVersion = Version

-- |This oracle is to depend on the upstream RSS without an intermediate file.
-- If the downloading failed, we don't want to fail our build.
-- URL -> Maybe T.Text
newtype UpstreamRSS = UpstreamRSS URL
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult UpstreamRSS = Maybe T.Text

main :: IO ()
main = withVersionAddendum $ do
  shakeDir <- fromMaybe "/var/lib/podripper/shake" <$> Env.lookupEnv "SHAKE_DIR"
  shakeArgs shakeOptions { shakeFiles = shakeDir, shakeColor = True } $ do
    want $ ["radiot", "rcmp"] <&> (<.> "rss")

    getRSSGenVersion <- addOracle $ \RSSGenVersion{} -> return Paths.version

    upstreamRSS <- addOracle $ \(UpstreamRSS url) -> do
      manager <- liftIO $ newManager tlsManagerSettings
      -- this `liftIO` is to fix the build error:
      -- • No instance for (exceptions-0.10.4:Control.Monad.Catch.MonadThrow
      --                      Action)
      --     arising from a use of ‘downloadRadioTRSS’
      liftIO $ runReaderT (runHTTPClientDownloadT $ downloadRSS url) manager

    versioned 16 $ "*.rss" %> \out -> do
      getRSSGenVersion $ RSSGenVersion ()

      configDir <- getEnvWithDefault "/usr/share/podripper" "CONF_DIR"
      let podcastTitle = dropExtension out
          feedConfigFile = configDir </> podcastTitle <> "_feed.json"
      need [feedConfigFile]
      feedConfig <- fmap decode . liftIO . BL.readFile $ feedConfigFile
      case feedConfig of
        Just config -> do
          conn <- liftIO openDatabase
          processUpstreamRSS upstreamRSS (T.pack podcastTitle) config conn
          generateFeed config conn out
          liftIO $ close conn
        Nothing -> fail $ "Couldn't parse feed config file " <> feedConfigFile

-- | Generates the feed at the requested path.
generateFeed :: RSSFeedConfig -> Connection -> FilePattern -> Action ()
generateFeed feedConfig conn out = do
  -- we need the audio files to generate the RSS, which are in the
  -- directory of the same name as the podcast title
  let podcastTitle = dropExtension out
  mp3Files <- getDirectoryFiles "" [podcastTitle </> "*.mp3"]
  let findUpstreamItem = closestUpstreamItemToTime (T.pack podcastTitle) conn
  rssItems <- liftIO . fmap (newestFirst . catMaybes) $ traverse (rssItemFromFile podcastTitle findUpstreamItem) mp3Files

  version <- askOracle $ RSSGenVersion ()
  writeFile' out $ feed (ProgramVersion . showVersion $ version) feedConfig rssItems

newestFirst :: [RSSItem] -> [RSSItem]
newestFirst = sortOn Down

-- | Downloads the upstream RSS from the URL in the config, parses it and
-- saves the items in the database.
-- Downloading is triggered every time when our RSS is requested, and
-- the RSS is not updated if the upstream RSS hasn't changed.
processUpstreamRSS :: (UpstreamRSS -> Action (Maybe T.Text)) -> UpstreamRSSFeed.PodcastId -> RSSFeedConfig -> Connection -> Action ()
processUpstreamRSS upstreamRSS podcastTitle config conn = void $ runMaybeT $ do
  url <- MaybeT . pure $ upstreamRSSURL config
  text <- MaybeT . upstreamRSS . UpstreamRSS . T.unpack $ url
  items <- MaybeT . pure . eitherToMaybe . UpstreamRSSFeed.parse podcastTitle $ text
  -- if we're here, then we have items
  liftIO $ saveUpstreamRSSItems conn items

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

downloadRSS :: MonadDownload m => URL -> m (Maybe T.Text)
downloadRSS = fmap (fmap (TE.decodeUtf8 . BL.toStrict)) . getFile

-- | Prints @rssgen@'s version prior to shake's version when `--version` is passed.
-- It's a workaround since shake doesn't support extending the output. Note that
-- shake also supports `-v` and `--numeric-version` for version output.
withVersionAddendum :: IO () -> IO ()
withVersionAddendum rest = printVersionWhenRequested >> rest
  where
    printVersionWhenRequested = do
      args <- Env.getArgs
      case args of
        ["--version"] -> putStrLn $ "rssgen " <> showVersion Paths.version
        _ -> pure ()
