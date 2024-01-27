module Rip
  ( parseRipDate
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

-- | Removes the suffix from the second string if present, and returns the second
-- string otherwise.
-- This is different from @Data.Text.stripSuffix@ as that one returns @Nothing@
-- if there is no match.
-- TODO extract the optionality to separate functions?
maybeStripSuffix :: Eq a => [a] -> [a] -> [a]
maybeStripSuffix suffix s = if suffix `isSuffixOf` s
  then reverse . drop (length suffix) . reverse $ s
  else s
