module RSSGen.Duration
  ( Duration
  , durationHours
  , durationMinutes
  , parseDuration
  ) where

import Data.Attoparsec.Text
import Data.Time.Clock
import Data.Text (Text)

-- | A duration of time between two `UTCTime`s.
type Duration = NominalDiffTime

parseDuration :: Text -> Either String Duration
parseDuration = parseOnly durationParser

durationParser :: Parser Duration
durationParser = do
  n <- decimal
  unit <- satisfy $ inClass "sm"

  pure $ case unit of
    's' -> fromIntegral n
    'm' -> durationMinutes n
    _ -> error $ "impossible unit: " <> show unit

-- | Creates a `Duration` from the given number of minutes.
durationMinutes :: Int -> Duration
durationMinutes = realToFrac . (* 60)

-- | Creates a `Duration` from the given number of hours.
durationHours :: Int -> Duration
durationHours = durationMinutes . (* 60)
