module RSSGen.Duration
  ( Duration
  , durationHours
  , durationMinutes
  ) where

import Data.Time.Clock

-- | A duration of time between two `UTCTime`s.
type Duration = NominalDiffTime

-- | Creates a `Duration` from the given number of minutes.
durationMinutes :: Int -> Duration
durationMinutes = realToFrac . (* 60)

-- | Creates a `Duration` from the given number of hours.
durationHours :: Int -> Duration
durationHours = durationMinutes . (* 60)
