{-# LANGUAGE DeriveGeneric #-}

module RipConfig
  ( RipConfig(..)
  ) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics
import RSSGen.Duration

-- | Configuration necessary to rip a stream.
data RipConfig = RipConfig
  { streamURL :: !Text
  , duration :: !Duration
  , retrySec :: !Int
  , ripDirName :: !Text
  , podArtist :: !Text
  , podAlbum :: !Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON RipConfig
