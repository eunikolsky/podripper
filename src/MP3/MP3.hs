{-# LANGUAGE BinaryLiterals #-}

module MP3.MP3
  ( Bitrate(..)
  , SamplingRate(..)
  ) where

-- | Sampling rate of a frame; it's required to calculate the frame size in
-- bytes and frame duration in seconds.
data SamplingRate = SR32000Hz | SR44100Hz | SR48000Hz
  deriving stock Eq

instance Show SamplingRate where
  show SR32000Hz = "32 kHz"
  show SR44100Hz = "44.1 kHz"
  show SR48000Hz = "48 kHz"

-- | Bitrate of a frame; it's required to calculate the frame size in bytes.
data Bitrate
  = BR32kbps
  | BR40kbps
  | BR48kbps
  | BR56kbps
  | BR64kbps
  | BR80kbps
  | BR96kbps
  | BR112kbps
  | BR128kbps
  | BR160kbps
  | BR192kbps
  | BR224kbps
  | BR256kbps
  | BR320kbps
  deriving stock (Eq, Enum, Bounded)

instance Show Bitrate where
  show BR32kbps = "32 kb/s"
  show BR40kbps = "40 kb/s"
  show BR48kbps = "48 kb/s"
  show BR56kbps = "56 kb/s"
  show BR64kbps = "64 kb/s"
  show BR80kbps = "80 kb/s"
  show BR96kbps = "96 kb/s"
  show BR112kbps = "112 kb/s"
  show BR128kbps = "128 kb/s"
  show BR160kbps = "160 kb/s"
  show BR192kbps = "192 kb/s"
  show BR224kbps = "224 kb/s"
  show BR256kbps = "256 kb/s"
  show BR320kbps = "320 kb/s"
