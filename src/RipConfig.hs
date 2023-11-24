module RipConfig
  ( RipConfig(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import RSSGen.Duration
import Ripper.RipperDelay

-- | Configuration necessary to rip a stream.
data RipConfig = RipConfig
  { streamURL :: !Text
  , duration :: !Duration
  , retryDelay :: !RetryDelay
  -- FIXME remove `duration` and `retryDelay` when refactored
  , ripIntervalRefs :: ![RipperIntervalRef]
  , ripDirName :: !Text
  , podArtist :: !Text
  , podAlbum :: !Text
  }
  deriving (Show, Eq)

instance FromJSON RipConfig where
  parseJSON = withObject "RipConfig" $ \o -> RipConfig
    <$> o .: "streamURL"
    <*> o .: "duration"
    <*> o .: "retryDelay"
    -- the key is called `ripIntervals` instead of `ripIntervalRefs` because
    -- `RipperIntervalRef` is an internal workaround for pure parsers and
    -- doesn't need to leak outside
    <*> o .: "ripIntervals"
    <*> o .: "ripDirName"
    <*> o .: "podArtist"
    <*> o .: "podAlbum"
