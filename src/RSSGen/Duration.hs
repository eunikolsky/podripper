module RSSGen.Duration
  ( Duration(..)
  , RetryDelay(..)
  , durationHours
  , durationMinutes
  , parseDuration
  , toNominalDiffTime
  ) where

import Data.Aeson
import Data.Attoparsec.Text
import Data.Time.Clock
import Data.Text (Text)

-- | A second-precision duration of time between two `UTCTime`s.
newtype Duration = Duration { toSeconds :: Int }
  deriving newtype (Eq)

instance Show Duration where
  show = (<> "s") . show . toSeconds

instance FromJSON Duration where
  parseJSON = withText "Duration" $ either fail pure . parseDuration

-- | Converts the `Duration` to `NominalDiffTime`.
toNominalDiffTime :: Duration -> NominalDiffTime
toNominalDiffTime = realToFrac . toSeconds

-- | Parses a time duration from a format that includes a number and a time
-- unit, e.g. `42s`, `30m`, `12h`.
parseDuration :: Text -> Either String Duration
parseDuration = parseOnly durationParser

durationParser :: Parser Duration
durationParser = do
  n <- decimal
  unit <- satisfy $ inClass "smh"
  endOfInput

  pure $ case unit of
    's' -> Duration n
    'm' -> durationMinutes n
    'h' -> durationHours n
    _ -> error $ "impossible unit: " <> show unit

-- | Creates a `Duration` from the given number of minutes.
durationMinutes :: Int -> Duration
durationMinutes = Duration . (* 60)

-- | Creates a `Duration` from the given number of hours.
durationHours :: Int -> Duration
durationHours = durationMinutes . (* 60)

-- | Duration of time to sleep for between retries in `runUntil`.
--
-- (Is this separate type really necessary?)
newtype RetryDelay = RetryDelay { toDuration :: Duration }
  deriving newtype (Show, Eq, FromJSON)
