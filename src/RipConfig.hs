module RipConfig
  ( RipConfig(..)
  ) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics
import RSSGen.Duration
import Ripper.RipperDelay

-- | Configuration necessary to rip a stream.
data RipConfig = RipConfig
  { streamURL :: !Text
  , duration :: !Duration
  , retryDelay :: !RetryDelay
  -- FIXME remove `duration` and `retryDelay` when refactored
  , ripIntervals :: ![RipperIntervalRef]
  , ripDirName :: !Text
  , podArtist :: !Text
  , podAlbum :: !Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON RipConfig
