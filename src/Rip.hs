module Rip
  ( localTimeToZonedTime
  , parseRipDate
  , titlePubDate
  ) where

import Data.Function
import Data.List (isSuffixOf, stripPrefix)
import Data.Time
import System.FilePath.Posix

-- | Parses the rip time from the filename. Assumes the standard streamripper's
-- filename like `sr_program_2020_03_21_21_55_20_enc.mp3` (the `_enc` at the end
-- may be missing).
parseRipDate :: FilePath -> Maybe LocalTime
parseRipDate file = do
  dateString <- takeBaseName file
    & maybeStripSuffix "_enc"
    & stripPrefix "sr_program_"
  let acceptSurroundingWhitespace = False
  parseTimeM acceptSurroundingWhitespace defaultTimeLocale "%Y_%m_%d_%H_%M_%S" dateString

getTimeZoneAtLocalTime :: LocalTime -> IO TimeZone
getTimeZoneAtLocalTime localTime = do
  -- this probably introduces a bug in some corner cases where the
  -- timezone may be off one hour (EET/EEST)
  currentTZ <- getCurrentTimeZone
  let utcTime = localTimeToUTC currentTZ localTime
  getTimeZone utcTime

-- | Extends the local time with the local timezone /at that time/.
localTimeToZonedTime :: LocalTime -> IO (ZonedTime, UTCTime)
localTimeToZonedTime localTime = do
  -- we need to have UTCTime to convert it to a zoned time,
  -- but converting local time to UTC also requires a timezone
  localTZ <- getTimeZoneAtLocalTime localTime
  let utcTime = localTimeToUTC localTZ localTime
  pure (utcToZonedTime localTZ utcTime, utcTime)

-- | Formats the publication date for the RSS item's title, specifically in
-- the `YYYY-MM-DD` format so that the files appear sorted on the podcast
-- player when synced with gPodder (which renames the files on the device
-- based on the title).
titlePubDate :: ZonedTime -> String
titlePubDate = formatTime defaultTimeLocale "%F %T %z"

-- | Removes the suffix from the second string if present, and returns the second
-- string otherwise.
-- This is different from @Data.Text.stripSuffix@ as that one returns @Nothing@
-- if there is no match.
-- TODO extract the optionality to separate functions?
maybeStripSuffix :: Eq a => [a] -> [a] -> [a]
maybeStripSuffix suffix s = if suffix `isSuffixOf` s
  then reverse . drop (length suffix) . reverse $ s
  else s
