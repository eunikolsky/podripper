{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RSSItem where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Function
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

instance Eq RSSItem where
  (RSSItem file0 title0 fileSize0 ripTime0) == (RSSItem file1 title1 fileSize1 ripTime1) = and
    [ file0 == file1
    , title0 == title1
    , fileSize0 == fileSize1
    , zonedTimeToUTC ripTime0 == zonedTimeToUTC ripTime1
    ]

instance Ord RSSItem where
  -- is this a valid definition given that `Eq` compares all the fields?
  (<=) = (<=) `on` zonedTimeToUTC . ripTime

-- | Formats the publication date string as required in the RSS standard.
formatPubDate :: ZonedTime -> String
formatPubDate = formatTime defaultTimeLocale "%d %b %Y %H:%M:%S %z"

-- | Creates an @RSSItem@ based on the information about the file and the
-- podcast title. Returns @Nothing@ if the file is not found.
rssItemFromFile :: String -> FilePath -> IO (Maybe RSSItem)
rssItemFromFile podcastTitle filename = runMaybeT $ do
  file <- MaybeT $ doesFileExist' filename
  fileSize <- liftIO $ getFileSize file
  localTime <- MaybeT . pure . parseRipDate $ file
  ripTime <- liftIO $ localTimeToZonedTime localTime
  let title = T.pack . (++ " / " <> podcastTitle) . formatPubDate $ ripTime

  return $ RSSItem {..}

-- | Parses the rip time from the filename. Assumes the standard streamripper's
-- filename like `sr_program_2020_03_21_21_55_20_enc.mp3` (the `_enc` at the
-- end may be missing).
parseRipDate :: FilePath -> Maybe LocalTime
parseRipDate file = do
  dateString <- stripPrefix "sr_program_" . maybeStripSuffix "_enc" . takeBaseName $ file
  let acceptSurroundingWhitespace = False
  parseTimeM acceptSurroundingWhitespace defaultTimeLocale "%Y_%m_%d_%H_%M_%S" dateString

-- | Extends the local time with the local timezone /at that time/.
localTimeToZonedTime :: LocalTime -> IO ZonedTime
localTimeToZonedTime localTime = do
  currentTZ <- getCurrentTimeZone
  -- we need to have UTCTime to convert it to a zoned time,
  -- but converting local time to UTC also requires a timezone
  let utcTime = localTimeToUTC currentTZ localTime
  localTZ <- getTimeZone utcTime
  return . utcToZonedTime localTZ $ utcTime

-- | Renders the RSS item into an XML element for the feed. First parameter
-- is the base URL for the file.
renderItem :: String -> RSSItem -> Element
renderItem baseURL RSSItem {..} = unode "item" [ititle, guid, description, pubDate, enclosure]
  where
    ititle = unode "title" $ T.unpack title
    guid = unode "guid" (Attr (unqual "isPermaLink") "false", takeFileName file)
    description = unode "description" ()
    pubDate = unode "pubDate" $ formatPubDate ripTime
    enclosure = unode "enclosure"
      [ Attr (unqual "url") $ baseURL <> file
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
