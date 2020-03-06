module Main where

import Development.Shake
import Development.Shake.FilePath

main :: IO ()
main = shakeArgs shakeOptions $ do
  want ["radiot" <.> "rss"]

  "radiot" <.> "rss" %> \out -> do
    putInfo "Hello, world!"
    writeFile' out $ mconcat ["<!-- ", out, " -->"]
