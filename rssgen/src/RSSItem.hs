{-# LANGUAGE OverloadedStrings #-}

module RSSItem where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.List
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import System.Directory
import System.FilePath.Posix

-- | An item in an RSS feed, based on a present file.
data RSSItem = RSSItem
  { file :: FilePath
  , title :: T.Text
  , fileSize :: Integer
  , ripTime :: UTCTime
  }
  deriving (Show)

-- | Creates an @RSSItem@ based on the information about the file. Returns
-- @Nothing@ if the file is not found.
rssItemFromFile :: FilePath -> IO (Maybe RSSItem)
rssItemFromFile file = runMaybeT $ do
  existingFile <- MaybeT $ doesFileExist' file
  fileSize <- liftIO $ getFileSize existingFile
  ripTime <- MaybeT . pure . parseRipDate $ existingFile

  return $ RSSItem
    { file = existingFile
    , title = T.pack . takeFileName $ existingFile
    , fileSize = fileSize
    , ripTime = ripTime
    }

-- | Parses the rip date from the filename. Assumes the standard streamripper's
-- filename like `sr_program_2020_03_21_21_55_20_enc.mp3` (the `_enc` at the
-- end may be missing).
parseRipDate :: FilePath -> Maybe UTCTime
parseRipDate file = do
  let baseName = takeBaseName file
  f <- stripPrefix "sr_program_" . maybeStripSuffix "_enc" $ baseName
  return $ UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

-- | Returns the filename if it exists.
doesFileExist' :: FilePath -> IO (Maybe FilePath)
doesFileExist' file = do
  exists <- doesFileExist file
  return $ if exists
    then Just file
    else Nothing

-- | Removes the suffix from the second string if present, and returns the second
-- string otherwise. This is different from @Data.Text.stripSuffix@ as that one
-- returns @Nothing@ if there is no match.
maybeStripSuffix :: Eq a => [a] -> [a] -> [a]
maybeStripSuffix suffix s = filter s
  where
    filter = if suffix `isSuffixOf` s
      then reverse . drop (length suffix) . reverse
      else id
