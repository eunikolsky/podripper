module Main where

import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.FilePath

import RSSFeed
import RSSItem

main :: IO ()
main = shakeArgs shakeOptions $ do
  want ["radiot" <.> "rss"]

  versioned 5 $ "radiot" <.> "rss" %> \out -> do
    -- we need the audio files to generate the RSS, which are in the
    -- directory of the same name as the podcast title
    let mp3Directory = dropExtension out
    mp3Files <- getDirectoryFiles "" [mp3Directory </> "*.mp3"]
    rssItems <- liftIO . fmap catMaybes $ traverse rssItemFromFile mp3Files
    writeFile' out $ feed rssItems
