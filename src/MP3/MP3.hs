{-# LANGUAGE BinaryLiterals #-}

module MP3.MP3
  ( Bitrate(..)
  , MPEG1SamplingRate(..)
  , MPEG2SamplingRate(..)
  , MPEGVersion(..)
  , SamplingRate(..)
  ) where

-- | MPEG version of a given MP3 frame; it's required to calculate the frame
-- size in bytes and frame duration in seconds (via samples/frame).
data MPEGVersion = MPEG1 | MPEG2
  deriving stock (Show, Eq)

data MPEG1SamplingRate = SR32000Hz | SR44100Hz | SR48000Hz
  deriving stock Eq

instance Show MPEG1SamplingRate where
  show SR32000Hz = "32 kHz"
  show SR44100Hz = "44.1 kHz"
  show SR48000Hz = "48 kHz"

data MPEG2SamplingRate = SR16000Hz | SR22050Hz | SR24000Hz
  deriving stock Eq

instance Show MPEG2SamplingRate where
  show SR16000Hz = "16 kHz"
  show SR22050Hz = "22.05 kHz"
  show SR24000Hz = "24 kHz"

-- | Sampling rate of a frame; it's required to calculate the frame size in
-- bytes and frame duration in seconds.
data SamplingRate
  -- sampling rate is stored as three values per MPEG version, which can be
  -- encoded in only 3 bits now (1 bit MPEG version and 2 bits sampling rate);
  -- it used to be stored as six values w/o MPEG versions, which could only be
  -- encoded in 4 bits (1 bit MPEG version and 3 bits sampling rate)
  = MPEG1SR !MPEG1SamplingRate
  | MPEG2SR !MPEG2SamplingRate
  deriving stock Eq

instance Show SamplingRate where
  show (MPEG1SR sr) = show sr
  show (MPEG2SR sr) = show sr

-- | Bitrate of a frame; it's required to calculate the frame size in bytes.
data Bitrate
  = BR8kbps
  | BR16kbps
  | BR24kbps
  | BR32kbps
  | BR40kbps
  | BR48kbps
  | BR56kbps
  | BR64kbps
  | BR80kbps
  | BR96kbps
  | BR112kbps
  | BR128kbps
  | BR144kbps
  | BR160kbps
  | BR192kbps
  | BR224kbps
  | BR256kbps
  | BR320kbps
  deriving stock (Eq, Enum)

instance Show Bitrate where
  show BR8kbps = "8 kb/s"
  show BR16kbps = "16 kb/s"
  show BR24kbps = "24 kb/s"
  show BR32kbps = "32 kb/s"
  show BR40kbps = "40 kb/s"
  show BR48kbps = "48 kb/s"
  show BR56kbps = "56 kb/s"
  show BR64kbps = "64 kb/s"
  show BR80kbps = "80 kb/s"
  show BR96kbps = "96 kb/s"
  show BR112kbps = "112 kb/s"
  show BR128kbps = "128 kb/s"
  show BR144kbps = "144 kb/s"
  show BR160kbps = "160 kb/s"
  show BR192kbps = "192 kb/s"
  show BR224kbps = "224 kb/s"
  show BR256kbps = "256 kb/s"
  show BR320kbps = "320 kb/s"
