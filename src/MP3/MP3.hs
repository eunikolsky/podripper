{-# LANGUAGE BinaryLiterals #-}

module MP3.MP3
  ( AudioDuration(..)
  , Frame(..)
  , FrameInfo
  , MaybeFrame(..)
  , fiBitrate
  , frameDuration
  , frameParser
  , frameSize
  , getFrameData
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
import Text.Printf

-- | Audio duration of an MP3 frame/file, in seconds.
newtype AudioDuration = AudioDuration { getAudioDuration :: Double }
  deriving newtype (Eq, Ord, Fractional, Num)

instance Show AudioDuration where
  show (AudioDuration d) = show d <> " s"

-- | Bytes of an MP3 frame: header + data. The reason for the separate type is
-- to have a more concise `Show` instance.
newtype FrameData = FrameData { getFrameData :: ByteString }

instance Show FrameData where
  show (FrameData d) = "<" <> show (BS.length d) <> " bytes>"

-- | Parsed information about one MP3 frame: sampling rate and bitrate packed
-- into one byte for efficient storage of frames while ripping.
-- Only sampling rate and its MPEG version are necessary to calculate frame's
-- duration in seconds; bitrate is necessary to calculate frame's size in bytes.
newtype FrameInfo = FrameInfo { packedFrameInfo :: Word8 }
  deriving stock (Show, Eq)

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

-- | One MP3 frame: information and all its bytes.
data Frame = Frame
  { fInfo :: !FrameInfo
  , fData :: !FrameData
  }
  deriving stock (Show)

type FrameSize = Int

-- | Used only for logging and easier debugging afterwards.
type JunkLength = Int

-- | Result of trying to parse an MP3 stream.
data MaybeFrame = Valid Frame | Junk JunkLength
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
  AudioDuration . (samplesPerFrame (srMPEGVersion samplingRate) /) $ samplingRateHz samplingRate
  where samplingRate = fiSamplingRate info

samplesPerFrame :: Num a => MPEGVersion -> a
samplesPerFrame MPEG1 = 1152
samplesPerFrame MPEG2 = 576

-- | Parses the header of an MP3 frame and returns its `FrameInfo`, header bytes
-- and the number of extra bytes to read for this frame.
frameHeaderParser :: Parser (FrameInfo, [Word8], Int)
frameHeaderParser = do
  let frameHeaderSize = 4
  bytes@[byte0, byte1, byte2, _] <- A.count frameHeaderSize A.anyWord8 <?> "Incomplete frame header"

  frameSyncValidator (byte0, byte1)
  mpegVersion <- parseMPEGVersion byte1
  layerValidator byte1

  bitrate <- bitrateParser mpegVersion byte2
  samplingRate <- samplingRateParser mpegVersion byte2

  let paddingSize = if testBit byte2 paddingBitIndex then 1 else 0
      contentsSize = frameSize' mpegVersion bitrate samplingRate - frameHeaderSize + paddingSize

  pure (mkFrameInfo samplingRate bitrate, bytes, contentsSize)

-- | Parses a single MP3 frame.
frameParser :: Parser Frame
frameParser = do
  (frameInfo, headerBytes, contentsSize) <- frameHeaderParser
  bytes <- A.take contentsSize
  pure Frame{fInfo=frameInfo, fData=FrameData $ BS.pack headerBytes <> bytes}

-- | Validates that the header bytes contain the valid frame sync.
-- It's called a validator because it returns unit (or error) since we don't
-- care about MPEG Version after this if it's valid.
frameSyncValidator :: (Word8, Word8) -> Parser ()
frameSyncValidator (b0, b1) =
  let isValid = (b0 == 0xff) && (b1 .&. byte1Mask == byte1Mask)
  in unless isValid . fail $ printf "Invalid frame sync (0x%02x%02x, %c%c)" b0 b1 b0 b1
  where byte1Mask = 0b1110_0000

-- | MPEG version of a given MP3 frame; it's required to calculate the frame
-- size in bytes and frame duration in seconds (via samples/frame).
data MPEGVersion = MPEG1 | MPEG2
  deriving stock (Show, Eq)

-- | Parses MPEG Version 1 or 2 from the header byte.
parseMPEGVersion :: Word8 -> Parser MPEGVersion
parseMPEGVersion byte = case 0b00000011 .&. byte `shiftR` 3 of
  0b11 -> pure MPEG1
  0b10 -> pure MPEG2
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

-- | Parses the sample rate from the frame byte.
samplingRateParser :: MPEGVersion -> Word8 -> Parser SamplingRate
samplingRateParser mpeg byte = case (mpeg, 0b00000011 .&. shiftR byte 2) of
  (MPEG1, 0b00) -> pure $ MPEG1SR SR44100Hz
  (MPEG1, 0b01) -> pure $ MPEG1SR SR48000Hz
  (MPEG1, 0b10) -> pure $ MPEG1SR SR32000Hz
  (MPEG2, 0b00) -> pure $ MPEG2SR SR22050Hz
  (MPEG2, 0b01) -> pure $ MPEG2SR SR24000Hz
  (MPEG2, 0b10) -> pure $ MPEG2SR SR16000Hz
  (_, 0b11) -> fail "Unexpected sampling rate \"reserved\" (3)"
  (_, x) -> fail $ "Impossible sampling rate value " <> show x

-- | Parses the bitrate from the frame byte.
bitrateParser :: MPEGVersion -> Word8 -> Parser Bitrate
bitrateParser mpeg byte = case (mpeg, shiftR byte 4) of
  (MPEG1, 0b0001) -> pure BR32kbps
  (MPEG1, 0b0010) -> pure BR40kbps
  (MPEG1, 0b0011) -> pure BR48kbps
  (MPEG1, 0b0100) -> pure BR56kbps
  (MPEG1, 0b0101) -> pure BR64kbps
  (MPEG1, 0b0110) -> pure BR80kbps
  (MPEG1, 0b0111) -> pure BR96kbps
  (MPEG1, 0b1000) -> pure BR112kbps
  (MPEG1, 0b1001) -> pure BR128kbps
  (MPEG1, 0b1010) -> pure BR160kbps
  (MPEG1, 0b1011) -> pure BR192kbps
  (MPEG1, 0b1100) -> pure BR224kbps
  (MPEG1, 0b1101) -> pure BR256kbps
  (MPEG1, 0b1110) -> pure BR320kbps
  (MPEG2, 0b0001) -> pure BR8kbps
  (MPEG2, 0b0010) -> pure BR16kbps
  (MPEG2, 0b0011) -> pure BR24kbps
  (MPEG2, 0b0100) -> pure BR32kbps
  (MPEG2, 0b0101) -> pure BR40kbps
  (MPEG2, 0b0110) -> pure BR48kbps
  (MPEG2, 0b0111) -> pure BR56kbps
  (MPEG2, 0b1000) -> pure BR64kbps
  (MPEG2, 0b1001) -> pure BR80kbps
  (MPEG2, 0b1010) -> pure BR96kbps
  (MPEG2, 0b1011) -> pure BR112kbps
  (MPEG2, 0b1100) -> pure BR128kbps
  (MPEG2, 0b1101) -> pure BR144kbps
  (MPEG2, 0b1110) -> pure BR160kbps
  (_, 0b0000) -> fail "Unexpected bitrate \"free\" (0)"
  (_, 0b1111) -> fail "Unexpected bitrate \"bad\" (15)"
  (_, x) -> fail $ "Impossible bitrate value " <> show x

-- | Returns the frame size based on the provided MPEG version, bitrate and
-- sample rate.
-- Note: most MP3 docs don't mention this, but the formula to calculate the
-- frame size is slightly different for MPEG2 vs MPEG1 Layer 3. See also:
-- https://stackoverflow.com/questions/62536328/mpeg-2-and-2-5-problems-calculating-frame-sizes-in-bytes/62539671#62539671
frameSize' :: MPEGVersion -> Bitrate -> SamplingRate -> FrameSize
frameSize' mpeg br sr = floor @Float $ samplesPerFrame mpeg / 8 * bitrateBitsPerSecond br / samplingRateHz sr
  where
    bitrateBitsPerSecond BR8kbps   = 8000
    bitrateBitsPerSecond BR16kbps  = 16000
    bitrateBitsPerSecond BR24kbps  = 24000
    bitrateBitsPerSecond BR32kbps  = 32000
    bitrateBitsPerSecond BR40kbps  = 40000
    bitrateBitsPerSecond BR48kbps  = 48000
    bitrateBitsPerSecond BR56kbps  = 56000
    bitrateBitsPerSecond BR64kbps  = 64000
    bitrateBitsPerSecond BR80kbps  = 80000
    bitrateBitsPerSecond BR96kbps  = 96000
    bitrateBitsPerSecond BR112kbps = 112000
    bitrateBitsPerSecond BR128kbps = 128000
    bitrateBitsPerSecond BR144kbps = 144000
    bitrateBitsPerSecond BR160kbps = 160000
    bitrateBitsPerSecond BR192kbps = 192000
    bitrateBitsPerSecond BR224kbps = 224000
    bitrateBitsPerSecond BR256kbps = 256000
    bitrateBitsPerSecond BR320kbps = 320000

frameSize :: FrameInfo -> FrameSize
frameSize info =
  frameSize' (srMPEGVersion samplingRate) bitrate samplingRate
  where
    samplingRate = fiSamplingRate info
    bitrate = fiBitrate info

srMPEGVersion :: SamplingRate -> MPEGVersion
srMPEGVersion (MPEG1SR _) = MPEG1
srMPEGVersion (MPEG2SR _) = MPEG2

samplingRateHz :: Num a => SamplingRate -> a
samplingRateHz (MPEG2SR SR16000Hz) = 16000
samplingRateHz (MPEG2SR SR22050Hz) = 22050
samplingRateHz (MPEG2SR SR24000Hz) = 24000
samplingRateHz (MPEG1SR SR32000Hz) = 32000
samplingRateHz (MPEG1SR SR44100Hz) = 44100
samplingRateHz (MPEG1SR SR48000Hz) = 48000

-- TODO try https://github.com/stevana/bits-and-bobs
paddingBitIndex :: Int
paddingBitIndex = 1
