{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.Functor
import Data.List
import Data.Maybe
import Data.Ord (Down(..))
import Data.Version (Version, showVersion)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified System.Environment as Env

import qualified Paths_rssgen as Paths (version)
import Downloader
import RSSFeed
import RSSItem

newtype RSSGenVersion = RSSGenVersion ()
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult RSSGenVersion = Version

main :: IO ()
main = do
  shakeDir <- fromMaybe "/var/lib/podripper/shake" <$> Env.lookupEnv "SHAKE_DIR"
  shakeArgs shakeOptions { shakeFiles = shakeDir, shakeColor = True } $ do
    want $ ["radiot", "rcmp"] <&> (<.> "rss")

    getRSSGenVersion <- addOracle $ \(RSSGenVersion _) -> return Paths.version

    versioned 12 $ "*.rss" %> \out -> do
      getRSSGenVersion $ RSSGenVersion ()

      configDir <- getEnvWithDefault "/usr/share/podripper" "CONF_DIR"
      let podcastTitle = dropExtension out
          feedConfigFile = configDir </> podcastTitle <> "_feed.conf"
      need [feedConfigFile]
      feedConfig <- fmap decode . liftIO $ BL.readFile feedConfigFile
      case feedConfig of
        Just config -> generateFeed config out
        Nothing -> fail $ "Couldn't parse feed config file " <> feedConfigFile

    versioned 2 $ "_radiot.rss" %> \out -> do
      manager <- liftIO $ newManager tlsManagerSettings
      -- this `liftIO` is to fix the build error:
      -- • No instance for (exceptions-0.10.4:Control.Monad.Catch.MonadThrow
      --                      Action)
      --     arising from a use of ‘downloadRadioTRSS’
      maybeRSS <- liftIO $ runReaderT (runHTTPClientDownloadT downloadRadioTRSS) manager
      case maybeRSS of
        Just rss -> writeFileChanged out $ CL.unpack rss
        Nothing -> putWarn $ "Can't download " <> out

      where
        generateFeed :: RSSFeedConfig -> FilePattern -> Action ()
        generateFeed feedConfig out = do
          -- we need the audio files to generate the RSS, which are in the
          -- directory of the same name as the podcast title
          let podcastTitle = dropExtension out
          mp3Files <- getDirectoryFiles "" [podcastTitle </> "*.mp3"]
          rssItems <- liftIO . fmap (newestFirst . catMaybes) $ traverse (rssItemFromFile podcastTitle) mp3Files

          version <- askOracle $ RSSGenVersion ()
          writeFile' out $ feed (ProgramVersion . showVersion $ version) feedConfig rssItems

        newestFirst :: [RSSItem] -> [RSSItem]
        newestFirst = sortOn Down

downloadRadioTRSS :: MonadDownload m => m (Maybe Bytes)
downloadRadioTRSS = getFile "https://radio-t.com/podcast.rss"
