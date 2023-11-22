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

-- this avoids the crash of partial `minimum` below
getRipperDelay [] _ _ = defaultDelay

getRipperDelay intervals Nothing localNow = maybe limitedDefaultDelay riDelay $ find nowWithinInterval intervals
  where
    nowWithinInterval interval = nextWeekdayIsToday interval && nowWithinTimeInterval interval
    nextWeekdayIsToday interval = let today = todayInIntervalTZ interval in
      firstDayOfWeekOnAfter (riWeekday interval) today == today
    nowWithinTimeInterval interval = localTimeOfDay (nowInIntervalTZ interval) `between` riTimeInterval interval
    todayInIntervalTZ = localDay . nowInIntervalTZ
    nowInIntervalTZ = tzTimeLocalTime . tzNowInIntervalTZ
    -- this is the passed `localNow` (in the program's current timezone)
    -- converted to the interval's timezone
    tzNowInIntervalTZ interval = inTZ (toTZInfo $ riTZ interval) localNow

    -- This limits the default delay so that it's not longer than the time to
    -- the next ripper interval.
    -- Why? Assuming the default delay to be quite big (10 minutes) and an
    -- interval delay to be small (10 seconds) because a live podcast starts on
    -- time (imagine that!), asking for a delay right before the interval start
    -- would return the big delay, missing the start. This avoids that by
    -- returning a delay only big enough to wake up after the interval start.
    -- Why is only the default delay limited? Because the assumption is that
    -- it's big and all other delays are much smaller.
    limitedDefaultDelay = defaultDelay `noBiggerThan` delayToNextInterval
    delayToNextInterval = RetryDelay . durationSeconds . ceiling $ diffTimeToNextInterval + 1
    diffTimeToNextInterval = minimum $ fmap delayToInterval intervals
    delayToInterval i = let now = tzNowInIntervalTZ i in
      -- this `head` is safe because we always have at least one future
      -- (positive) time diff; using `NonEmpty.filter` wouldn't improve anything
      findNearestFutureDiff
        $ (`diffTZTime` now)
        . (\weekDiff -> modifyLocal (makeIntervalStartTime i weekDiff) now)
        <$> [thisWeek, nextWeek]
    findNearestFutureDiff = head . filter (> 0)
    makeIntervalStartTime i weekDiff now' = LocalTime
      { localDay = addWeeks weekDiff $ firstDayOfWeekOnAfter (riWeekday i) (localDay now')
      , localTimeOfDay = fst . riTimeInterval $ i
      }

    addWeeks w = addDays $ w * 7
    (thisWeek, nextWeek) = (0, 1)

between :: Ord a => a -> (a, a) -> Bool
x `between` (from, to) = x >= from && x <= to

noBiggerThan :: Ord a => a -> a -> a
noBiggerThan = min

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
