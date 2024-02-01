{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module MP3.FrameInfo
  ( FrameInfo
  , fiBitrate
  , fiChannel
  , fiPadding
  , fiSamplingRate
  , mkFrameInfo
  ) where

import Data.Bits
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Word
import MP3.MP3

-- | Parsed information about one MP3 frame: channel information, sampling rate,
-- bitrate and padding packed into two bytes for efficient storage of frames
-- while ripping.
-- Only sampling rate is necessary to calculate frame's duration in seconds;
-- bitrate and padding are necessary to calculate frame's size in bytes. Channel
-- information is used to mimic the existing frame for the Xing header.
newtype FrameInfo = FrameInfo { packedFrameInfo :: Word16 }
  deriving stock (Show, Eq)

-- boilerplate from the `Data.Vector.Unboxed` docs to be able to store
-- `FrameInfo` in an unboxed vector
newtype instance VU.MVector s FrameInfo = MV_Word16 (VU.MVector s Word16)
newtype instance VU.Vector FrameInfo = V_Word16 (VU.Vector Word16)
deriving newtype instance VGM.MVector VU.MVector FrameInfo
deriving newtype instance VG.Vector VU.Vector FrameInfo
instance VU.Unbox FrameInfo

-- Bit schema: `p_ccss_bbbb` where `p` is padding bit, `c` are channel bits,
-- `s` are sampling rate bits and `b` are bitrate bits.
mkFrameInfo :: SamplingRate -> Bitrate -> Channel -> Padding -> FrameInfo
mkFrameInfo sr br ch pad = FrameInfo . getIor . foldMap Ior $
  [ packPadding pad `shiftL` 8
  , packChannel ch `shiftL` 6
  , packSamplingRate sr `shiftL` 4
  , packBitrate br
  ]

  where
    packPadding = fromIntegral . fromEnum

    packChannel = fromIntegral . fromEnum

    packSamplingRate SR32000Hz = 0b00
    packSamplingRate SR44100Hz = 0b01
    packSamplingRate SR48000Hz = 0b10

    packBitrate = fromIntegral . fromEnum

fiSamplingRate :: FrameInfo -> SamplingRate
fiSamplingRate (FrameInfo word) = case (word `shiftR` 4) .&. 0b0000_0011 of
  0b00 -> SR32000Hz
  0b01 -> SR44100Hz
  0b10 -> SR48000Hz
  x -> error $ "Impossible FrameInfo sampling rate value " <> show x

fiBitrate :: FrameInfo -> Bitrate
fiBitrate (FrameInfo word) = toEnum . fromIntegral $ word .&. 0b0000_1111

fiChannel :: FrameInfo -> Channel
fiChannel (FrameInfo word) = toEnum . fromIntegral $ word `shiftR` 6 .&. 0b0000_0011

fiPadding :: FrameInfo -> Padding
fiPadding (FrameInfo word) = toEnum . fromIntegral $ word `shiftR` 8 .&. 0b0000_0001
