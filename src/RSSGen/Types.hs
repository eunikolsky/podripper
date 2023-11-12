{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RSSGen.Types
  ( Duration
  , Hours(..)
  , RetryDelay(..)
  , durationHours
  , durationMinutes
  ) where

import Data.Aeson
import Data.Time.Clock

-- | Represents an integer number of hours.
newtype Hours = Hours { getHours :: Int }
  deriving (Eq, FromJSON, ToJSON)

instance Show Hours where
  show (Hours h) = mconcat [show h, " hour", if h == 1 then "" else "s"]

-- | A duration of time between two `UTCTime`s.
type Duration = NominalDiffTime

-- | Creates a `Duration` from the given number of minutes.
durationMinutes :: Int -> Duration
durationMinutes = realToFrac . (* 60)

-- | Creates a `Duration` from the given number of hours.
durationHours :: Int -> Duration
durationHours = durationMinutes . (* 60)

-- | Duration of time to sleep for between retries in `runUntil`.
--
-- (Is this separate type really necessary?)
newtype RetryDelay = RetryDelay { toDuration :: Duration }
  deriving newtype (Show, Eq, FromJSON)
