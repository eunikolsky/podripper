{-# LANGUAGE BinaryLiterals #-}

module MP3.Generator
  ( frameForContentsSize
  , generateFrame
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word
import MP3.FrameInfo
import MP3.MP3
import MP3.Parser

-- | Generates an MPEG1 Layer3 frame with the header from `FrameInfo` and the
-- `contents`.
-- Warning: it's the responsibility of the caller to make sure the `contents`'
-- length matches the expected length for the given frame (use
-- `frameContentsSize` from `MP3.Parser` to get the length).
generateFrame :: FrameInfo -> ByteString -> ByteString
generateFrame info contents = BS.pack headerBytes <> contents
  where
    headerBytes =
      [ 0xff
      , 0b1111_1011
      -- no padding
      , bitrate (fiBitrate info) .|. samplingRate (fiSamplingRate info)
      , channel (fiChannel info) .|. 0b0100
      ]

frameForContentsSize :: FrameInfo -> FrameContentsSize -> Maybe FrameInfo
frameForContentsSize frame size | frameContentsSize frame >= size = Just frame
frameForContentsSize _ _ = Nothing

bitrate :: Bitrate -> Word8
bitrate br = (`shiftL` 4) $ case br of
  BR32kbps  -> 0b0001
  BR40kbps  -> 0b0010
  BR48kbps  -> 0b0011
  BR56kbps  -> 0b0100
  BR64kbps  -> 0b0101
  BR80kbps  -> 0b0110
  BR96kbps  -> 0b0111
  BR112kbps -> 0b1000
  BR128kbps -> 0b1001
  BR160kbps -> 0b1010
  BR192kbps -> 0b1011
  BR224kbps -> 0b1100
  BR256kbps -> 0b1101
  BR320kbps -> 0b1110

samplingRate :: SamplingRate -> Word8
samplingRate sr = (`shiftL` 2) $ case sr of
  SR44100Hz -> 0b00
  SR48000Hz -> 0b01
  SR32000Hz -> 0b10

channel :: Channel -> Word8
channel ch = (`shiftL` 6) $ case ch of
  Stereo      -> 0b00
  JointStereo -> 0b01
  Dual        -> 0b10
  Mono        -> 0b11
