{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.FilePath

import RSSFeed
import RSSItem

feedConfig :: RSSFeedConfig
feedConfig = RSSFeedConfig
  { RSSFeed.title = "Поток"
  , RSSFeed.description = "foo"
  , RSSFeed.language = "ru"
  , RSSFeed.podcastLink = "https://example.com/"
  , RSSFeed.imageLink = "https://example.com/cover.jpg"
  , RSSFeed.selfLink = "https://example.com/sample.rss"
  }

main :: IO ()
main = shakeArgs shakeOptions $ do
  want ["radiot" <.> "rss"]

  versioned 6 $ "radiot" <.> "rss" %> \out -> do
    -- we need the audio files to generate the RSS, which are in the
    -- directory of the same name as the podcast title
    let mp3Directory = dropExtension out
    mp3Files <- sort <$> getDirectoryFiles "" [mp3Directory </> "*.mp3"]
    rssItems <- liftIO . fmap catMaybes $ traverse rssItemFromFile mp3Files
    writeFile' out $ feed feedConfig rssItems
