module Ripper.RipperDelay
  ( RipperInterval
  , getRipperDelay
  , mkRipperInterval
  ) where

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
  }
  deriving Show

mkRipperInterval :: DayOfWeek -> (TimeOfDay, TimeOfDay) -> TZInfo -> Maybe RipperInterval
mkRipperInterval d ti@(from, to) tz = if from < to then Just (RipperInterval d ti $ TZ tz) else Nothing

type RipEndTime = TZTime
type Now = TZTime

getRipperDelay :: RipperInterval -> Maybe RipEndTime -> Now -> RetryDelay
getRipperDelay _ (Just ripEndTime) now = if now `diffTZTime` ripEndTime <= minutes 5
  then shortAfterRipDelay
  else longerAfterRipDelay
getRipperDelay interval Nothing localNow = if nowWithinInterval then standardIntervalDelay else defaultDelay
  where
    nowWithinInterval = nextWeekdayIsToday && nowWithinTimeInterval
    nextWeekdayIsToday = firstDayOfWeekOnAfter (riWeekday interval) today == today
    nowWithinTimeInterval = localTimeOfDay now `between` riTimeInterval interval
    today = localDay now
    -- this is the passed `localNow` (in the program's current timezone)
    -- converted to the interval's timezone
    now = tzTimeLocalTime $ inTZ (toTZInfo $ riTZ interval) localNow

between :: Ord a => a -> (a, a) -> Bool
x `between` (from, to) = x >= from && x <= to

-- A very short ripper delay that happens soon after a rip has ended because we
-- don't know whether the stream has ended or there was an error and it will be
-- back soon.
shortAfterRipDelay :: RetryDelay
shortAfterRipDelay = RetryDelay $ durationSeconds 1

longerAfterRipDelay :: RetryDelay
longerAfterRipDelay = RetryDelay $ durationSeconds 3

standardIntervalDelay :: RetryDelay
standardIntervalDelay = RetryDelay $ durationSeconds 20

-- | The default ripper delay if no other rule matches.
defaultDelay :: RetryDelay
defaultDelay = RetryDelay $ durationMinutes 10
