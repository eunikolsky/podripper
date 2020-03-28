{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.FilePath

import RSSFeed
import RSSItem

main :: IO ()
main = shakeArgs shakeOptions $ do
  want ["radiot" <.> "rss"]

  versioned 7 $ "radiot" <.> "rss" %> \out -> do
    let podcastTitle = dropExtension out
        feedConfigFile = podcastTitle <> "_feed.conf"
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
        mp3Files <- sort <$> getDirectoryFiles "" [podcastTitle </> "*.mp3"]
        rssItems <- liftIO . fmap catMaybes $ traverse rssItemFromFile mp3Files
        writeFile' out $ feed feedConfig rssItems
