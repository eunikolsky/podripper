{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module MP3.FrameInfo
  ( FrameInfo
  , fiBitrate
  , fiSamplingRate
  , mkFrameInfo
  ) where

import Data.Bits
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Word
import MP3.MP3

-- | Parsed information about one MP3 frame: sampling rate and bitrate packed
-- into one byte for efficient storage of frames while ripping.
-- Only sampling rate and its MPEG version are necessary to calculate frame's
-- duration in seconds; bitrate is necessary to calculate frame's size in bytes.
newtype FrameInfo = FrameInfo { packedFrameInfo :: Word8 }
  deriving stock (Show, Eq)

-- boilerplate from the `Data.Vector.Unboxed` docs to be able to store
-- `FrameInfo` in an unboxed vector
newtype instance VU.MVector s FrameInfo = MV_Word8 (VU.MVector s Word8)
newtype instance VU.Vector FrameInfo = V_Word8 (VU.Vector Word8)
deriving newtype instance VGM.MVector VU.MVector FrameInfo
deriving newtype instance VG.Vector VU.Vector FrameInfo
instance VU.Unbox FrameInfo

mkFrameInfo :: SamplingRate -> Bitrate -> FrameInfo
mkFrameInfo sr br = FrameInfo $ (packSamplingRate sr `shiftL` 5) .|. packBitrate br
  where
    packSamplingRate (MPEG1SR SR32000Hz) = 0b000
    packSamplingRate (MPEG1SR SR44100Hz) = 0b001
    packSamplingRate (MPEG1SR SR48000Hz) = 0b010
    packSamplingRate (MPEG2SR SR16000Hz) = 0b100
    packSamplingRate (MPEG2SR SR22050Hz) = 0b101
    packSamplingRate (MPEG2SR SR24000Hz) = 0b110

    packBitrate = fromIntegral . fromEnum

fiSamplingRate :: FrameInfo -> SamplingRate
fiSamplingRate (FrameInfo byte) = case byte `shiftR` 5 of
  0b000 -> MPEG1SR SR32000Hz
  0b001 -> MPEG1SR SR44100Hz
  0b010 -> MPEG1SR SR48000Hz
  0b100 -> MPEG2SR SR16000Hz
  0b101 -> MPEG2SR SR22050Hz
  0b110 -> MPEG2SR SR24000Hz
  x -> error $ "Impossible FrameInfo sampling rate value " <> show x

fiBitrate :: FrameInfo -> Bitrate
fiBitrate (FrameInfo byte) = toEnum . fromIntegral $ byte .&. 0b0001_1111

