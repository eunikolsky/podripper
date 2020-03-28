{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RSSItem where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.List
import qualified Data.Text as T
import Data.Time
import System.Directory
import System.FilePath.Posix
import Text.XML.Light

-- | An item in an RSS feed, based on a present file.
data RSSItem = RSSItem
  { file :: FilePath
  , title :: T.Text
  , fileSize :: Integer
  , ripTime :: ZonedTime
  }
  deriving (Show)

-- | Creates an @RSSItem@ based on the information about the file. Returns
-- @Nothing@ if the file is not found.
rssItemFromFile :: FilePath -> IO (Maybe RSSItem)
rssItemFromFile filename = runMaybeT $ do
  file <- MaybeT $ doesFileExist' filename
  let title = T.pack . takeFileName $ file
  fileSize <- liftIO $ getFileSize file
  localTime <- MaybeT . pure . parseRipDate $ file
  ripTime <- liftIO $ localTimeToZonedTime localTime

  return $ RSSItem {..}

-- | Parses the rip time from the filename. Assumes the standard streamripper's
-- filename like `sr_program_2020_03_21_21_55_20_enc.mp3` (the `_enc` at the
-- end may be missing).
parseRipDate :: FilePath -> Maybe LocalTime
parseRipDate file = do
  dateString <- stripPrefix "sr_program_" . maybeStripSuffix "_enc" . takeBaseName $ file
  let acceptSurroundingWhitespace = False
  parseTimeM acceptSurroundingWhitespace defaultTimeLocale "%Y_%m_%d_%H_%M_%S" dateString

-- | Adds the current timezone to the local time.
localTimeToZonedTime :: LocalTime -> IO ZonedTime
localTimeToZonedTime localTime = do
  tz <- getCurrentTimeZone
  return . utcToZonedTime tz . localTimeToUTC tz $ localTime

-- | Renders the RSS item into an XML element for the feed.
renderItem :: RSSItem -> Element
renderItem RSSItem {..} = unode "item" [ititle, guid, description, pubDate, enclosure]
  where
    ititle = unode "title" $ T.unpack title
    guid = unode "guid" (Attr (unqual "isPermaLink") "false", takeBaseName file)
    description = unode "description" ()
    pubDate = unode "pubDate" $ formatTime defaultTimeLocale "%d %b %Y %H:%M:%S %z" ripTime
    enclosure = unode "enclosure"
      [ Attr (unqual "url") $ "https://example.com/" <> file
      , Attr (unqual "type") "audio/mp3"
      , Attr (unqual "length") $ show fileSize
      ]

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
