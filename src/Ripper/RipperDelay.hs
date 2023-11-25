module Ripper.RipperDelay
  ( Now
  , RipEndTime
  , RipperInterval
  , RipperIntervalRef(..)
  , getRipperDelay
  , mkRipperInterval
  , parseRipperIntervalRef
  , riDelay
  , ripperIntervalFromRef
  ) where

import Control.Exception
import Control.Monad.Except
import Data.Aeson hiding ((<?>))
import Data.Attoparsec.Text
import Data.Foldable
import Data.Functor
import Data.Text (Text)
import Data.Time
import Data.Time.TZInfo
import Data.Time.TZTime
import RSSGen.Duration

-- | Timezone — wrapper around `TZInfo` that `show`s only the identifier
-- (without a long list of rules).
newtype TZ = TZ { toTZInfo :: TZInfo }
  deriving newtype Eq

instance Show TZ where
  show = show . tziIdentifier . toTZInfo

-- | The first step of parsing a `RipperInterval` that doesn't require any `IO`,
-- unlike `RipperInterval`, which needs `IO` to load the timezone.
--
-- See also: https://stackoverflow.com/questions/46810998/give-a-default-value-for-fields-not-available-in-json-using-aeson
data RipperIntervalRef = RipperIntervalRef
  { rirWeekday :: !DayOfWeek
  , rirTimeInterval :: !(TimeOfDay, TimeOfDay)
  , rirTZ :: !TZIdentifier
  , rirDelay :: !RetryDelay
  }
  deriving (Show, Eq)

-- | Time interval that provides a specific ripper delay, overriding the default
-- delay. It's used for podcasts that have known live recording times and we
-- want to record them from the beginning. Since they are usually weekly, this
-- interval is based on a time-of-day interval in the given timezone on a
-- certain day of week.
-- Note that the time interval currently isn't expected to overflow to the next
-- day, that is `(23:00:00, 01:30:00)` is unsupported.
data RipperInterval = RipperInterval
  { riWeekday :: !DayOfWeek
  , riTimeInterval :: !(TimeOfDay, TimeOfDay)
  -- ^ `from` is always less than `to`
  , riTZ :: !TZ
  , riDelay :: !RetryDelay
  -- ^ the delay to use when this interval is matched
  }
  deriving (Show, Eq)

-- | Smart constructor for `RipperInterval` that ensures that the time interval
-- is ascending (`from ≥ to`).
mkRipperInterval :: DayOfWeek -> (TimeOfDay, TimeOfDay) -> TZInfo -> RetryDelay -> Maybe RipperInterval
mkRipperInterval d ti@(from, to) tz delay = if from < to then Just (RipperInterval d ti (TZ tz) delay) else Nothing

type RipEndTime = TZTime
type Now = TZTime

-- | Determines the delay before the next ripping attempt. The rules are:
-- * within 5 minutes after latest rip ended => 1 second;
-- * within 15 minutes after latest rip ended => 3 seconds;
-- * if `now` is inside a ripper interval => use its specific delay;
-- * otherwise => 10 minutes, but so that it's not longer than the time to
-- the next ripper interval [0].
--
-- [0] Why? Assuming the default delay to be quite big (10 minutes) and an
-- interval delay to be small (10 seconds) because a live podcast starts on
-- time (imagine that!), asking for a delay right before the interval start
-- would return the big delay, missing the start. This avoids that by
-- returning a delay only big enough to wake up after the interval start.
-- Why is only the default delay limited? Because the assumption is that
-- it's big and all other delays are much smaller.
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

-- A very short ripper delay soon after a rip has ended because we don't know
-- whether the stream has ended or there was an error and it will be back soon.
shortAfterRipDelay :: RetryDelay
shortAfterRipDelay = RetryDelay $ durationSeconds 1

-- A longer ripper delay after a rip has ended that may still catch some live
-- stream.
longerAfterRipDelay :: RetryDelay
longerAfterRipDelay = RetryDelay $ durationSeconds 3

-- | The default ripper delay if no other rule matches.
defaultDelay :: RetryDelay
defaultDelay = RetryDelay $ durationMinutes 10

-- | Parses a `RipperIntervalRef` with the following format:
-- `dw h0:m0-h1:m1 timezone: delay`, for example
-- `Su 12:59-23:48 America/New_York: 9m`.
parseRipperIntervalRef :: Text -> Either String RipperIntervalRef
parseRipperIntervalRef = parseOnly pRipperIntervalRef

instance FromJSON RipperIntervalRef where
  parseJSON = withText "RipperIntervalRef" $ either fail pure . parseRipperIntervalRef

-- | Tries to convert `RipperIntervalRef` to `RipperInterval`.
ripperIntervalFromRef :: RipperIntervalRef -> IO (Either String RipperInterval)
ripperIntervalFromRef RipperIntervalRef{rirWeekday,rirTimeInterval,rirTZ,rirDelay} = runExceptT $ do
  tz <- ExceptT $ loadTZ rirTZ
  liftEither . justInterval $ mkRipperInterval rirWeekday rirTimeInterval tz rirDelay

  where
    loadTZ :: TZIdentifier -> IO (Either String TZInfo)
    loadTZ = handle failedTZ . fmap Right . loadFromSystem

    failedTZ :: IOException -> IO (Either String a)
    failedTZ e = pure . Left $ "Failed to load timezone: " <> show e

    justInterval (Just i) = Right i
    justInterval Nothing = Left "Couldn't parse time interval"

pRipperIntervalRef :: Parser RipperIntervalRef
pRipperIntervalRef = do
  weekday <- pWeekday <?> "weekday"
  skipChar ' '
  from <- pTime <?> "from time"
  skipChar '-'
  to <- pTime <?> "to time"
  skipChar ' '
  tzId <- takeWhile1 (/= ':') <?> "timezone id"
  void $ string ": "
  delay <- retryDurationParser <?> "retry duration"
  pure $ RipperIntervalRef weekday (from, to) tzId delay

pWeekday :: Parser DayOfWeek
pWeekday = choice
  [ string "Mo" $> Monday
  , string "Tu" $> Tuesday
  , string "We" $> Wednesday
  , string "Th" $> Thursday
  , string "Fr" $> Friday
  , string "Sa" $> Saturday
  , string "Su" $> Sunday
  ]

pTime :: Parser TimeOfDay
pTime = do
  hour <- pTwoDigitNumber
  skipChar ':'
  minute <- pTwoDigitNumber
  pure $ TimeOfDay hour minute 0

pTwoDigitNumber :: Parser Int
pTwoDigitNumber = read <$> count 2 digit

skipChar :: Char -> Parser ()
skipChar = void . char
