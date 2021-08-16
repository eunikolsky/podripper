{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RSSItem where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Function
import Data.List
import qualified Data.Text as T
import Data.Time
import System.Directory
import System.FilePath.Posix
import Text.XML.Light

import qualified UpstreamRSSFeed

-- | An item in an RSS feed, based on a present file.
data RSSItem = RSSItem
  { file :: FilePath
  , title :: T.Text
  , fileSize :: Integer
  , ripTime :: ZonedTime
  , description :: Maybe T.Text
  }
  deriving (Show)

instance Eq RSSItem where
  (RSSItem file0 title0 fileSize0 ripTime0 description0) == (RSSItem file1 title1 fileSize1 ripTime1 description1) = and
    [ file0 == file1
    , title0 == title1
    , fileSize0 == fileSize1
    , zonedTimeToUTC ripTime0 == zonedTimeToUTC ripTime1
    , description0 == description1
    ]

instance Ord RSSItem where
  -- is this a valid definition given that `Eq` compares all the fields?
  (<=) = (<=) `on` zonedTimeToUTC . ripTime

-- | Formats the publication date string as required in the RSS standard.
formatPubDate :: ZonedTime -> String
formatPubDate = formatTime defaultTimeLocale "%d %b %Y %H:%M:%S %z"

-- | Formats the publication date for the RSS item's title, specifically in
-- the `YYYY-MM-DD` format so that the files appear sorted on the podcast
-- player when synced with gPodder (which renames the files on the device
-- based on the title).
titlePubDate :: ZonedTime -> String
titlePubDate = formatTime defaultTimeLocale "%F %T %z"

-- | Creates an @RSSItem@ based on the information about the file. If
-- @upstreamItems@ contains an RSS item close to the item's date, uses its
-- title, otherwise uses the @podcastTitle@. Returns @Nothing@ if the file
-- is not found.
rssItemFromFile :: String -> [UpstreamRSSFeed.UpstreamRSSItem] -> FilePath -> IO (Maybe RSSItem)
rssItemFromFile podcastTitle upstreamItems filename = runMaybeT $ do
  file <- MaybeT $ doesFileExist' filename
  fileSize <- liftIO $ getFileSize file
  localTime <- MaybeT . pure . parseRipDate $ file
  (ripTime, utcTime) <- liftIO $ localTimeToZonedTime localTime
  let maybeUpstreamItem = closestUpstreamItemToTime upstreamItems utcTime
  let titleSuffix = maybe podcastTitle (T.unpack . UpstreamRSSFeed.title) maybeUpstreamItem
  let title = T.pack . (++ " / " <> titleSuffix) . titlePubDate $ ripTime
  let description = UpstreamRSSFeed.description <$> maybeUpstreamItem

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
localTimeToZonedTime :: LocalTime -> IO (ZonedTime, UTCTime)
localTimeToZonedTime localTime = do
  currentTZ <- getCurrentTimeZone
  -- we need to have UTCTime to convert it to a zoned time,
  -- but converting local time to UTC also requires a timezone
  let utcTime = localTimeToUTC currentTZ localTime
  localTZ <- getTimeZone utcTime
  return (utcToZonedTime localTZ utcTime, utcTime)

-- | Renders the RSS item into an XML element for the feed. First parameter
-- is the base URL for the file.
renderItem :: String -> RSSItem -> Element
renderItem baseURL RSSItem {..} = unode "item" [ititle, guid, idescription, pubDate, enclosure]
  where
    ititle = unode "title" $ T.unpack title
    guid = unode "guid" (Attr (unqual "isPermaLink") "false", takeFileName file)
    idescription = maybe (unode "description" ()) (unode "description" . flip (CData CDataVerbatim) Nothing . T.unpack) description
    pubDate = unode "pubDate" $ formatPubDate ripTime
    enclosure = unode "enclosure"
      [ Attr (unqual "url") $ baseURL <> file
      , Attr (unqual "type") "audio/mp3"
      , Attr (unqual "length") $ show fileSize
      ]

-- | Returns an upstream RSS item closest to @time@ if it's within one day.
closestUpstreamItemToTime :: [UpstreamRSSFeed.UpstreamRSSItem] -> UTCTime -> Maybe UpstreamRSSFeed.UpstreamRSSItem
closestUpstreamItemToTime items time = do
  guardNonEmpty items
  let closeItems = filter (withinOneDay time) items
  guardNonEmpty closeItems
  return $ maximumBy (compare `on` UpstreamRSSFeed.pubDate) closeItems

  where
    withinOneDay :: UTCTime -> UpstreamRSSFeed.UpstreamRSSItem -> Bool
    withinOneDay time item = (< nominalDay) . abs . diffUTCTime time $ UpstreamRSSFeed.pubDate item

    guardNonEmpty :: Alternative f => [a] -> f ()
    guardNonEmpty = guard . not . null

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
