module Ripper.RipperDelay
  ( RipperInterval
  , getRipperDelay
  , mkRipperInterval
  , riDelay
  ) where

import Data.Foldable
import Data.Time
import Data.Time.TZInfo
import Data.Time.TZTime
import RSSGen.Duration

-- | Timezone — wrapper around `TZInfo` that `show`s only the identifier
-- (without a long list of rules).
newtype TZ = TZ { toTZInfo :: TZInfo }

instance Show TZ where
  show = show . tziIdentifier . toTZInfo

data RipperInterval = RipperInterval
  { riWeekday :: !DayOfWeek
  , riTimeInterval :: !(TimeOfDay, TimeOfDay)
  -- ^ `from` is always less than `to`
  , riTZ :: !TZ
  , riDelay :: !RetryDelay
  -- ^ the delay to use when this interval is matched
  }
  deriving Show

mkRipperInterval :: DayOfWeek -> (TimeOfDay, TimeOfDay) -> TZInfo -> RetryDelay -> Maybe RipperInterval
mkRipperInterval d ti@(from, to) tz delay = if from < to then Just (RipperInterval d ti (TZ tz) delay) else Nothing

type RipEndTime = TZTime
type Now = TZTime

getRipperDelay :: [RipperInterval] -> Maybe RipEndTime -> Now -> RetryDelay
getRipperDelay intervals (Just ripEndTime) now
  | timeSinceRipEnd <= minutes 5 = shortAfterRipDelay
  | timeSinceRipEnd <= minutes 15 = longerAfterRipDelay
  -- not immediately after a rip => check for intervals
  | otherwise = getRipperDelay intervals Nothing now

  where timeSinceRipEnd = now `diffTZTime` ripEndTime

getRipperDelay intervals Nothing localNow = maybe defaultDelay riDelay $ find nowWithinInterval intervals
  where
    nowWithinInterval interval = nextWeekdayIsToday interval && nowWithinTimeInterval interval
    nextWeekdayIsToday interval = let today = todayInIntervalTZ interval in
      firstDayOfWeekOnAfter (riWeekday interval) today == today
    nowWithinTimeInterval interval = localTimeOfDay (nowInIntervalTZ interval) `between` riTimeInterval interval
    todayInIntervalTZ = localDay . nowInIntervalTZ
    -- this is the passed `localNow` (in the program's current timezone)
    -- converted to the interval's timezone
    nowInIntervalTZ interval = tzTimeLocalTime $ inTZ (toTZInfo $ riTZ interval) localNow

between :: Ord a => a -> (a, a) -> Bool
x `between` (from, to) = x >= from && x <= to

-- A very short ripper delay that happens soon after a rip has ended because we
-- don't know whether the stream has ended or there was an error and it will be
-- back soon.
shortAfterRipDelay :: RetryDelay
shortAfterRipDelay = RetryDelay $ durationSeconds 1

longerAfterRipDelay :: RetryDelay
longerAfterRipDelay = RetryDelay $ durationSeconds 3

-- | The default ripper delay if no other rule matches.
defaultDelay :: RetryDelay
defaultDelay = RetryDelay $ durationMinutes 10
