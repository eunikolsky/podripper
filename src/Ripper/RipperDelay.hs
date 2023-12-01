module Ripper.RipperDelay
  ( Now
  , PostRipEndDelay(..)
  , RipEndTime
  , RipperInterval
  , RipperIntervalRef(..)
  , getRipperDelay
  , mkRipperInterval
  , parsePostRipEndDelay
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
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
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
  deriving (Eq)

instance Show RipperIntervalRef where
  show RipperIntervalRef{rirWeekday,rirTimeInterval,rirTZ,rirDelay} = mconcat
    [ abbreviatedWeekday rirWeekday, " "
    , hoursMinutes $ fst rirTimeInterval, "-"
    , hoursMinutes $ snd rirTimeInterval, " "
    , T.unpack rirTZ, ": "
    , show rirDelay
    ]

    where
      abbreviatedWeekday Monday = "Mo"
      abbreviatedWeekday Tuesday = "Tu"
      abbreviatedWeekday Wednesday = "We"
      abbreviatedWeekday Thursday = "Th"
      abbreviatedWeekday Friday = "Fr"
      abbreviatedWeekday Saturday = "Sa"
      abbreviatedWeekday Sunday = "Su"

      hoursMinutes = formatTime defaultTimeLocale "%R"

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

data PostRipEndDelay = PostRipEndDelay
  { prdSinceRipEndLimit :: !Duration
  -- ^ when less time than this value has passed since the latest rip ended…
  , prdDelay :: !RetryDelay
  -- ^ …use this delay
  }
  deriving (Eq)

instance Show PostRipEndDelay where
  show PostRipEndDelay{prdSinceRipEndLimit,prdDelay} = mconcat
    [ "[< ", show prdSinceRipEndLimit
    , "]: ", show prdDelay
    ]

type RipEndTime = TZTime
type Now = TZTime

-- | Determines the delay before the next ripping attempt. The rules are:
--
-- * if `now` is within some specific duration since latest rip ended => use the
-- corresponding delay; the list of `PostRipEndDelay`s is used for these checks;
-- the list should be ordered by the increasing `prdSinceRipEndLimit` for the
-- checks to work correctly;
--
-- * if `now` is inside a ripper interval => use its specific delay;
--
-- * otherwise => `defaultDelay`, but so that it's not longer than the time to
-- the next ripper interval [0].
--
-- [0] Why? Assuming the default delay to be quite big (10 minutes) and an
-- interval delay to be small (10 seconds) because a live podcast starts on
-- time (imagine that!), asking for a delay right before the interval start
-- would return the big delay, missing the start. This avoids that by
-- returning a delay only big enough to wake up after the interval start.
-- Why is only the default delay limited? Because the assumption is that
-- it's big and all other delays are much smaller.
getRipperDelay :: (RetryDelay, [PostRipEndDelay]) -> [RipperInterval] -> Maybe RipEndTime -> Now -> RetryDelay
getRipperDelay defaults@(_, postRipEndDelays) intervals (Just ripEndTime) now =
  fromMaybe getIntervalsDelay getPostRipEndDelay

  where
    getPostRipEndDelay = prdDelay <$> find
      (\PostRipEndDelay{prdSinceRipEndLimit} -> timeSinceRipEnd <= toNominalDiffTime prdSinceRipEndLimit)
      postRipEndDelays
    timeSinceRipEnd = now `diffTZTime` ripEndTime
    -- not immediately after a rip => check for intervals
    getIntervalsDelay = getRipperDelay defaults intervals Nothing now

-- this avoids the crash of partial `minimum` below
getRipperDelay (defaultDelay, _) [] _ _ = defaultDelay

getRipperDelay (defaultDelay, _) intervals Nothing localNow = maybe limitedDefaultDelay riDelay $ find nowWithinInterval intervals
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

-- | Parses a `RipperIntervalRef` with the following format:
-- `dw h0:m0-h1:m1 timezone: delay`, for example
-- `Su 12:59-23:48 America/New_York: 9m`.
parseRipperIntervalRef :: Text -> Either String RipperIntervalRef
parseRipperIntervalRef = parseOnly $ pRipperIntervalRef <* endOfInput

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

-- | Parses a `PostRipEndDelay` with the following format:
-- `[< dur]: dur`, for example `[< 5m]: 1s`.
parsePostRipEndDelay :: Text -> Either String PostRipEndDelay
parsePostRipEndDelay = parseOnly $ pPostRipEndDelay <* endOfInput

instance FromJSON PostRipEndDelay where
  parseJSON = withText "PostRipEndDelay" $ either fail pure . parsePostRipEndDelay

pPostRipEndDelay :: Parser PostRipEndDelay
pPostRipEndDelay = do
  void $ string "[< "
  limit <- durationParser
  void $ string "]: "
  delay <- retryDurationParser
  pure $ PostRipEndDelay {prdSinceRipEndLimit = limit, prdDelay = delay}
