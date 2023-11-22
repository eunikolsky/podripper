module RSSGen.Duration
  ( Duration
  , RetryDelay(..)
  , durationHours
  , durationMinutes
  , durationSeconds
  , parseDuration
  , toMicroseconds
  , toNominalDiffTime
  , toSeconds
  ) where

import Data.Aeson hiding ((<?>))
import Data.Attoparsec.Text
import Data.Function
import Data.Time.Clock
import Data.Text (Text)
import Development.Shake.Classes
import GHC.Generics

-- | A second-precision duration of time between two `UTCTime`s.
data Duration = Seconds Int | Minutes Int | Hours Int
  deriving (Eq, Generic, Hashable, Binary, NFData)

instance Show Duration where
  show (Seconds s) = show s <> "s"
  show (Minutes m) = show m <> "m"
  show (Hours h) = show h <> "h"

instance Ord Duration where
  compare = compare `on` toSeconds

instance FromJSON Duration where
  parseJSON = withText "Duration" $ either fail pure . parseDuration

-- | Converts the `Duration` to `NominalDiffTime`.
toNominalDiffTime :: Duration -> NominalDiffTime
toNominalDiffTime = realToFrac . toSeconds

toSeconds :: Duration -> Int
toSeconds (Seconds s) = s
toSeconds (Minutes m) = m * 60
toSeconds (Hours h) = h * 60 * 60

-- | Converts the `Duration` to the number of microseconds for the `threadDelay`
-- function.
--
-- 64-bit `Int` should be enough to represent 28 hours.
toMicroseconds :: Duration -> Int
toMicroseconds = (* microsecondsInSecond) . toSeconds

microsecondsInSecond :: Num a => a
microsecondsInSecond = 1_000_000

-- | Parses a time duration from a format that includes a number and a time
-- unit, e.g. `42s`, `30m`, `12h`.
parseDuration :: Text -> Either String Duration
parseDuration = parseOnly durationParser

durationParser :: Parser Duration
durationParser = do
  n <- decimal <?> "duration amount"
  unit <- satisfy (inClass "smh") <?> "duration unit"
  endOfInput

  pure $ case unit of
    's' -> durationSeconds n
    'm' -> durationMinutes n
    'h' -> durationHours n
    _ -> error $ "impossible unit: " <> show unit

-- | Creates a `Duration` from the given number of seconds.
durationSeconds :: Int -> Duration
-- TODO simplify to `Minutes`/`Hours` if divisible
durationSeconds = Seconds

-- | Creates a `Duration` from the given number of minutes.
durationMinutes :: Int -> Duration
durationMinutes = Minutes

-- | Creates a `Duration` from the given number of hours.
durationHours :: Int -> Duration
durationHours = Hours

-- | Duration of time to sleep for between retries in `runUntil`.
--
-- (Is this separate type really necessary?)
newtype RetryDelay = RetryDelay { toDuration :: Duration }
  deriving newtype (Show, Eq, Ord, FromJSON, Hashable, Binary, NFData)
