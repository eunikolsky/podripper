{-# LANGUAGE DeriveGeneric #-}

module RipConfig
  ( RipConfig(..)
  ) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics

-- | Configuration necessary to rip a stream.
data RipConfig = RipConfig
  { streamURL :: !Text
    -- TODO introduce a `Duration` type, which can parse strings like "2h"
  , durationSec :: !Int
  , retrySec :: !Int
  , ripDirName :: !Text
  , podArtist :: !Text
  , podAlbum :: !Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON RipConfig
