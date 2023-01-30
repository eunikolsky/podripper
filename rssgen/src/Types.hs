{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
  ( Hours(..)
  ) where

import Data.Aeson

-- | Represents an integer number of hours.
newtype Hours = Hours { getHours :: Int }
  deriving (Eq, FromJSON, ToJSON)

instance Show Hours where
  show (Hours h) = mconcat [show h, " hour", if h == 1 then "" else "s"]
