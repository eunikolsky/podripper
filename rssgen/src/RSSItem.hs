{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RSSItem where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Strict
import Data.Function
import Data.List
import Data.Maybe (fromMaybe)
import Data.Monoid (Any(..))
import qualified Data.Text as T
import Data.Time
import System.Directory
import System.FilePath.Posix
import Text.XML.Light

import qualified UpstreamRSSFeed

-- | Specifies the type of the rip file (based on the filename).
data RipType
  -- | A rip file that has been reencoded and should have the correct MP3 headers.
  = ReencodedRip
  -- | Source rip file for which reeconding failed and seeking inside it in players is likely broken.
  | SourceRip
  deriving (Eq, Show)

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

-- | Creates an @RSSItem@ based on the information about the file.
-- @findUpstreamItem@ is used to find an upstream RSS item that is close
-- to the item's date; if there is one, uses its title, otherwise uses the
-- @podcastTitle@. Returns @Nothing@ if the file is not found.
rssItemFromFile :: String -> (UTCTime -> IO (Maybe UpstreamRSSFeed.UpstreamRSSItem)) -> FilePath -> IO (Maybe RSSItem)
rssItemFromFile podcastTitle findUpstreamItem filename = runMaybeT $ do
  file <- MaybeT $ doesFileExist' filename
  fileSize <- liftIO $ getFileSize file
  (localTime, ripType) <- MaybeT . pure . parseRipDate $ file
  (ripTime, utcTime) <- liftIO $ localTimeToZonedTime localTime
  -- note: can't use the `MaybeT IO` monad of this `do` block because
  -- a missing upstream item is not an error that should cause a `Nothing`
  (title, description) <- MaybeT . fmap pure $ do
    maybeUpstreamItem <- findUpstreamItem utcTime
    let { title = T.pack $ mconcat
      [ if ripType == SourceRip then "SOURCE " else ""
      , titlePubDate ripTime
      , " / "
      , (T.unpack . UpstreamRSSFeed.title <$> maybeUpstreamItem) ?? podcastTitle
      ]
    }
    let description = UpstreamRSSFeed.description <$> maybeUpstreamItem
    pure (title, description)

  return $ RSSItem {..}

-- | `Maybe`-coalescing operator.
(??) :: Maybe a -> a -> a
(??) = flip fromMaybe

-- | Parses the rip time from the filename. Assumes the standard streamripper's
-- filename like `sr_program_2020_03_21_21_55_20_enc.mp3` (the `_enc` or `_src`
-- at the end may be missing).
parseRipDate :: FilePath -> Maybe (LocalTime, RipType)
parseRipDate file = fmap mapResult . runWriterT $ do
  {-
  the type of the value returned by this block is `WriterT Any Maybe LocalTime`;
  the `Any` monoid calculates the final flag: `False` by default and can be set to
  to `True` only based on the presence of the `_src` suffix

  x :: MaybeT (Writer Flag) String
  runMaybeT x :: Writer Flag (Maybe String)
  runWriter $ runMaybeT x :: (Maybe String, Flag)

  x :: WriterT Flag Maybe String
  runWriterT x :: Maybe (String, Flag)
  -}

  dateString <- takeBaseName file
    & writer . fmap Any . maybeStripSuffix "_src"
    <$> fst . maybeStripSuffix "_enc"
    >>= lift . stripPrefix "sr_program_"
  let acceptSurroundingWhitespace = False
  parseTimeM acceptSurroundingWhitespace defaultTimeLocale "%Y_%m_%d_%H_%M_%S" dateString

  where
    mapResult :: (LocalTime, Any) -> (LocalTime, RipType)
    mapResult = fmap (ripTypeBySrcSuffix . getAny)

    ripTypeBySrcSuffix :: Bool -> RipType
    ripTypeBySrcSuffix True = SourceRip
    ripTypeBySrcSuffix False = ReencodedRip

localTimeToZonedTime' :: TimeZone -> LocalTime -> IO (ZonedTime, UTCTime)
localTimeToZonedTime' currentTZ localTime = do
  -- we need to have UTCTime to convert it to a zoned time,
  -- but converting local time to UTC also requires a timezone
  localTZ <- getTimeZoneAtLocalTime currentTZ localTime
  let utcTime = localTimeToUTC localTZ localTime
  return (utcToZonedTime localTZ utcTime, utcTime)

getTimeZoneAtLocalTime :: TimeZone -> LocalTime -> IO TimeZone
getTimeZoneAtLocalTime currentTZ localTime = do
  -- TODO do we need currentTZ?
  let utcTime = localTimeToUTC currentTZ localTime
  getTimeZone utcTime

-- | Extends the local time with the local timezone /at that time/.
localTimeToZonedTime :: LocalTime -> IO (ZonedTime, UTCTime)
localTimeToZonedTime localTime = getCurrentTimeZone >>= flip localTimeToZonedTime' localTime

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

-- | Returns the filename if it exists.
doesFileExist' :: FilePath -> IO (Maybe FilePath)
doesFileExist' file = do
  exists <- doesFileExist file
  return $ if exists
    then Just file
    else Nothing

-- | Removes the suffix from the second string if present, and returns the second
-- string otherwise; also returns whether the suffix was present.
-- This is different from @Data.Text.stripSuffix@ as that one returns @Nothing@
-- if there is no match.
-- TODO extract the optionality to separate functions?
maybeStripSuffix :: Eq a => [a] -> [a] -> ([a], Bool)
maybeStripSuffix suffix s = if suffix `isSuffixOf` s
  then (reverse . drop (length suffix) . reverse $ s, True)
  else (s, False)
