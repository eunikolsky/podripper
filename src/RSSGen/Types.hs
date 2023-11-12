{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RSSGen.Types
  ( Hours(..)
  , RetryDelay(..)
  ) where

import Data.Aeson
import RSSGen.Duration

-- | Represents an integer number of hours.
newtype Hours = Hours { getHours :: Int }
  deriving (Eq, FromJSON, ToJSON)

instance Show Hours where
  show (Hours h) = mconcat [show h, " hour", if h == 1 then "" else "s"]

-- | Duration of time to sleep for between retries in `runUntil`.
--
-- (Is this separate type really necessary?)
newtype RetryDelay = RetryDelay { toDuration :: Duration }
  deriving newtype (Show, Eq, FromJSON)
