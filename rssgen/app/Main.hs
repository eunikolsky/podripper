{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Reader
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
import qualified UpstreamRSSFeed

newtype RSSGenVersion = RSSGenVersion ()
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult RSSGenVersion = Version

newtype PodcastName = PodcastName String
  deriving (Show, Eq, Hashable, Binary, NFData)

-- |This oracle is to depend on the upstream RSS without an intermediate file.
-- If the downloading failed, we don't want to fail our build.
-- PodcastName -> Maybe T.Text
newtype UpstreamRSS = UpstreamRSS PodcastName
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult UpstreamRSS = Maybe T.Text

main :: IO ()
main = do
  shakeDir <- fromMaybe "/var/lib/podripper/shake" <$> Env.lookupEnv "SHAKE_DIR"
  shakeArgs shakeOptions { shakeFiles = shakeDir, shakeColor = True } $ do
    want $ ["radiot", "rcmp"] <&> (<.> "rss")

    getRSSGenVersion <- addOracle $ \RSSGenVersion{} -> return Paths.version

    upstreamRSS <- addOracle $ \(UpstreamRSS _) -> do
      manager <- liftIO $ newManager tlsManagerSettings
      -- this `liftIO` is to fix the build error:
      -- • No instance for (exceptions-0.10.4:Control.Monad.Catch.MonadThrow
      --                      Action)
      --     arising from a use of ‘downloadRadioTRSS’
      liftIO $ runReaderT (runHTTPClientDownloadT downloadRadioTRSS) manager

    versioned 14 $ "*.rss" %> \out -> do
      getRSSGenVersion $ RSSGenVersion ()

      configDir <- getEnvWithDefault "/usr/share/podripper" "CONF_DIR"
      let podcastTitle = dropExtension out
          feedConfigFile = configDir </> podcastTitle <> "_feed.conf"
      need [feedConfigFile]
      feedConfig <- fmap decode . liftIO . BL.readFile $ feedConfigFile
      -- downloading is triggered every time when this RSS is requested, and
      -- the RSS is not updated if the upstream RSS hasn't changed
      maybeUpstreamRSSBytes <- upstreamRSS . UpstreamRSS . PodcastName $ podcastTitle
      case feedConfig of
        Just config -> do
          let maybeUpstreamRSS = maybeUpstreamRSSBytes >>= UpstreamRSSFeed.parse
          generateFeed config (fromMaybe [] maybeUpstreamRSS) out
        Nothing -> fail $ "Couldn't parse feed config file " <> feedConfigFile

    where
      generateFeed :: RSSFeedConfig -> [UpstreamRSSFeed.UpstreamRSSItem] -> FilePattern -> Action ()
      generateFeed feedConfig upstreamItems out = do
        -- we need the audio files to generate the RSS, which are in the
        -- directory of the same name as the podcast title
        let podcastTitle = dropExtension out
        mp3Files <- getDirectoryFiles "" [podcastTitle </> "*.mp3"]
        rssItems <- liftIO . fmap (newestFirst . catMaybes) $ traverse (rssItemFromFile podcastTitle upstreamItems) mp3Files

        version <- askOracle $ RSSGenVersion ()
        writeFile' out $ feed (ProgramVersion . showVersion $ version) feedConfig rssItems

      newestFirst :: [RSSItem] -> [RSSItem]
      newestFirst = sortOn Down

downloadRadioTRSS :: MonadDownload m => m (Maybe T.Text)
downloadRadioTRSS = fmap (TE.decodeUtf8 . BL.toStrict) <$> getFile "https://radio-t.com/podcast.rss"
