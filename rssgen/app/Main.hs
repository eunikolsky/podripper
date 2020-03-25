module Main where

import Data.List
import Development.Shake
import Development.Shake.FilePath

main :: IO ()
main = shakeArgs shakeOptions $ do
  want ["radiot" <.> "rss"]

  versioned 3 $ "radiot" <.> "rss" %> \out -> do
    -- we need the audio files to generate the RSS, which are in the
    -- directory of the same name as the podcast title
    let mp3Directory = dropExtension out
    mp3Files <- getDirectoryFiles "" [mp3Directory </> "*.mp3"]
    writeFile' out $ mconcat ["<!-- ", intercalate ", " mp3Files, " -->"]
