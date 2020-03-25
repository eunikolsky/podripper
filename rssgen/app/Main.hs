module Main where

import Data.List
import Development.Shake
import Development.Shake.FilePath

main :: IO ()
main = shakeArgs shakeOptions $ do
  want ["radiot" <.> "rss"]

  versioned 1 $ "radiot" <.> "rss" %> \out -> do
    -- we need the audio files to generate the RSS
    mp3Files <- getDirectoryFiles "" ["*.mp3"]
    writeFile' out $ mconcat ["<!-- ", intercalate ", " mp3Files, " -->"]
