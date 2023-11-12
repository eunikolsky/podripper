module RSSGen.Duration
  ( Duration
  , durationHours
  , durationMinutes
  , parseDuration
  ) where

import Data.Time.Clock
import Data.Text (Text)

-- | A duration of time between two `UTCTime`s.
type Duration = NominalDiffTime

parseDuration :: Text -> Maybe Duration
parseDuration = const Nothing

-- | Creates a `Duration` from the given number of minutes.
durationMinutes :: Int -> Duration
durationMinutes = realToFrac . (* 60)

-- | Creates a `Duration` from the given number of hours.
durationHours :: Int -> Duration
durationHours = durationMinutes . (* 60)
