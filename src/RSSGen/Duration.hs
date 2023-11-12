module RSSGen.Duration
  ( Duration(..)
  , durationHours
  , durationMinutes
  , parseDuration
  ) where

import Data.Aeson
import Data.Attoparsec.Text
import Data.Time.Clock
import Data.Text (Text)

-- | A duration of time between two `UTCTime`s.
newtype Duration = Duration { toNominalDiffTime :: NominalDiffTime }
  deriving newtype (Show, Eq)

instance FromJSON Duration where
  parseJSON = withText "Duration" $ either fail pure . parseDuration

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
    's' -> Duration $ fromIntegral n
    'm' -> durationMinutes n
    'h' -> durationHours n
    _ -> error $ "impossible unit: " <> show unit

-- | Creates a `Duration` from the given number of minutes.
durationMinutes :: Int -> Duration
durationMinutes = Duration . realToFrac . (* 60)

-- | Creates a `Duration` from the given number of hours.
durationHours :: Int -> Duration
durationHours = durationMinutes . (* 60)
