{-# LANGUAGE BinaryLiterals #-}

module MP3.Parser
  ( AudioDuration(..)
  , Frame(..)
  , FrameData(..)
  , FrameInfo
  , MaybeFrame(..)
  , fiBitrate
  , frameContentsSize
  , frameDuration
  , frameHeaderSize
  , frameParser
  , frameSize
  , maybeFrameParser
  ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString ((<?>), Parser)
import Data.Attoparsec.ByteString qualified as A
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word
import MP3.AudioDuration
import MP3.FrameInfo
import MP3.MP3
import Text.Printf

-- | Bytes of an MP3 frame: header + data. The reason for the separate type is
-- to have a more concise `Show` instance.
newtype FrameData = FrameData { getFrameData :: ByteString }
  deriving newtype Eq

instance Show FrameData where
  show (FrameData d) = "<" <> show (BS.length d) <> " bytes>"

-- | One MP3 frame: information and all its bytes.
data Frame = Frame
  { fInfo :: !FrameInfo
  , fData :: !FrameData
  }
  deriving stock (Show)

type FrameSize = Word16

-- | Used only for logging and easier debugging afterwards.
type JunkLength = Int

-- | Result of trying to parse an MP3 stream.
data MaybeFrame = Valid !Frame | Junk !JunkLength
  deriving stock Show

-- | Parser for MP3 streams that either parses a valid frame, or some junk until
-- something that looks like a valid frame start (so the next parse result may
-- also be junk).
--
-- Junk is likely to appear at the start when you dump an internet MP3 stream
-- connecting at an arbitrary point in time. I don't think ID3 tags are present
-- in such streams (ICY metadata can be used instead). I suppose if a stream
-- ends successfully, the server (such as `icecast`) should send the complete
-- last frame. However in case of a disconnect, you're likely to receive a
-- partial frame, which is also junk.
maybeFrameParser :: Parser MaybeFrame
maybeFrameParser = validFrame <|> junk
  where
    validFrame = Valid <$> frameParser

    junk = do
      -- `anyWord8` before `skipWhile` is crucial here to avoid an infinite loop
      -- that happens because `skipWhile` never does anything after it reaches
      -- `0xff`; if that's the current byte and `frameParser` couldn't use it, it's
      -- junk, so we skip it
      void A.anyWord8
      let skippedByte = 1
      bs <- A.takeWhile (/= 0xff)
      pure . Junk $ BS.length bs + skippedByte

frameDuration :: FrameInfo -> AudioDuration
frameDuration info =
  AudioDuration . (samplesPerFrame /) $ samplingRateHz samplingRate
  where samplingRate = fiSamplingRate info

samplesPerFrame :: Num a => a
samplesPerFrame = 1152

frameHeaderSize :: Num a => a
frameHeaderSize = 4

-- | Parses the header of an MP3 frame and returns its `FrameInfo`, header bytes
-- and the number of extra bytes to read for this frame.
frameHeaderParser :: Parser (FrameInfo, [Word8], FrameSize)
frameHeaderParser = do
  bytes@[byte0, byte1, byte2, byte3] <- A.count frameHeaderSize A.anyWord8 <?> "Incomplete frame header"

  frameSyncValidator (byte0, byte1)
  validateMPEG1 byte1
  layerValidator byte1

  bitrate <- bitrateParser byte2
  samplingRate <- samplingRateParser byte2
  channel <- channelParser byte3

  let paddingSize = if testBit byte2 paddingBitIndex then 1 else 0
      contentsSize = frameSize' bitrate samplingRate - frameHeaderSize + paddingSize

  pure (mkFrameInfo samplingRate bitrate channel, bytes, contentsSize)

-- | Parses a single MP3 frame.
frameParser :: Parser Frame
frameParser = do
  (frameInfo, headerBytes, contentsSize) <- frameHeaderParser
  bytes <- A.take $ fromIntegral contentsSize
  pure Frame{fInfo=frameInfo, fData=FrameData $ BS.pack headerBytes <> bytes}

-- | Validates that the header bytes contain the valid frame sync.
-- It's called a validator because it returns unit (or error) since we don't
-- care about MPEG Version after this if it's valid.
frameSyncValidator :: (Word8, Word8) -> Parser ()
frameSyncValidator (b0, b1) =
  let isValid = (b0 == 0xff) && (b1 .&. byte1Mask == byte1Mask)
  in unless isValid . fail $ printf "Invalid frame sync (0x%02x%02x, %c%c)" b0 b1 b0 b1
  where byte1Mask = 0b1110_0000

-- | Validates that the header byte declares MPEG Version 1.
validateMPEG1 :: Word8 -> Parser ()
validateMPEG1 byte = case 0b00000011 .&. byte `shiftR` 3 of
  0b11 -> pure ()
  0b10 -> fail "Unsupported MPEG version 2 (2) frame"
  0b00 -> fail "Unexpected MPEG version 2.5 (0) frame"
  0b01 -> fail "Unexpected MPEG version \"reserved\" (1) frame"
  x -> fail $ "Impossible MPEG version value " <> show x

-- | Validates that the header byte declares Layer 3.
layerValidator :: Word8 -> Parser ()
layerValidator byte = case 0b0000_0011 .&. byte `shiftR` 1 of
  0b01 -> pure ()
  0b11 -> fail "Unexpected Layer 1 (3) frame"
  0b10 -> fail "Unexpected Layer 2 (2) frame"
  0b00 -> fail "Unexpected Layer \"reserved\" (0) frame"
  x -> fail $ "Impossible Layer value " <> show x

-- | Parses the sample rate from the frame byte.
samplingRateParser :: Word8 -> Parser SamplingRate
samplingRateParser byte = case 0b00000011 .&. shiftR byte 2 of
  0b00 -> pure SR44100Hz
  0b01 -> pure SR48000Hz
  0b10 -> pure SR32000Hz
  0b11 -> fail "Unexpected sampling rate \"reserved\" (3)"
  x -> fail $ "Impossible sampling rate value " <> show x

-- | Parses the bitrate from the frame byte.
bitrateParser :: Word8 -> Parser Bitrate
bitrateParser byte = case shiftR byte 4 of
  0b0001 -> pure BR32kbps
  0b0010 -> pure BR40kbps
  0b0011 -> pure BR48kbps
  0b0100 -> pure BR56kbps
  0b0101 -> pure BR64kbps
  0b0110 -> pure BR80kbps
  0b0111 -> pure BR96kbps
  0b1000 -> pure BR112kbps
  0b1001 -> pure BR128kbps
  0b1010 -> pure BR160kbps
  0b1011 -> pure BR192kbps
  0b1100 -> pure BR224kbps
  0b1101 -> pure BR256kbps
  0b1110 -> pure BR320kbps
  0b0000 -> fail "Unexpected bitrate \"free\" (0)"
  0b1111 -> fail "Unexpected bitrate \"bad\" (15)"
  x -> fail $ "Impossible bitrate value " <> show x

channelParser :: Word8 -> Parser Channel
channelParser byte = case byte `shiftR` 6 of
  0b00 -> pure Stereo
  0b01 -> pure JointStereo
  0b10 -> pure Dual
  0b11 -> pure Mono
  x -> fail $ "Impossible channel value " <> show x

-- | Returns the frame size (including the frame header!) based on the provided
-- bitrate and sampling rate.
frameSize' :: Bitrate -> SamplingRate -> FrameSize
frameSize' br sr = floor @Float $ samplesPerFrame / 8 * bitrateBitsPerSecond br / samplingRateHz sr
  where
    bitrateBitsPerSecond BR32kbps  = 32000
    bitrateBitsPerSecond BR40kbps  = 40000
    bitrateBitsPerSecond BR48kbps  = 48000
    bitrateBitsPerSecond BR56kbps  = 56000
    bitrateBitsPerSecond BR64kbps  = 64000
    bitrateBitsPerSecond BR80kbps  = 80000
    bitrateBitsPerSecond BR96kbps  = 96000
    bitrateBitsPerSecond BR112kbps = 112000
    bitrateBitsPerSecond BR128kbps = 128000
    bitrateBitsPerSecond BR160kbps = 160000
    bitrateBitsPerSecond BR192kbps = 192000
    bitrateBitsPerSecond BR224kbps = 224000
    bitrateBitsPerSecond BR256kbps = 256000
    bitrateBitsPerSecond BR320kbps = 320000

-- | Returns the frame size (including the frame header!) based on the provided
-- `FrameInfo`.
frameSize :: FrameInfo -> FrameSize
frameSize info =
  frameSize' bitrate samplingRate
  where
    samplingRate = fiSamplingRate info
    bitrate = fiBitrate info

-- | Returns the frame size (without the frame header!) based on the provided
-- `FrameInfo`.
frameContentsSize :: FrameInfo -> FrameContentsSize
frameContentsSize = (\x -> x - frameHeaderSize) . frameSize

samplingRateHz :: Num a => SamplingRate -> a
samplingRateHz SR32000Hz = 32000
samplingRateHz SR44100Hz = 44100
samplingRateHz SR48000Hz = 48000

-- TODO try https://github.com/stevana/bits-and-bobs
paddingBitIndex :: Int
paddingBitIndex = 1
