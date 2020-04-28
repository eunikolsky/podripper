{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Functor
import Data.List
import Data.Maybe
import Data.Ord (Down(..))
import Data.Version (Version, showVersion)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath

import qualified Paths_rssgen as Paths (version)
import RSSFeed
import RSSItem

newtype RSSGenVersion = RSSGenVersion ()
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult RSSGenVersion = Version

main :: IO ()
main = shakeArgs shakeOptions { shakeColor = True } $ do
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
