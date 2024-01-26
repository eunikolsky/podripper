{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}

module MP3.Xing
  ( MP3Structure(..)
  , XingHeader(..)
  , calculateXingHeader
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.Vector.Unboxed qualified as VU
import Data.Word
import MP3.Parser

-- TODO a better data structure for storing frames?
newtype MP3Structure = MP3Structure { unMP3Structure :: VU.Vector FrameInfo }
  deriving Eq

instance Show MP3Structure where
  -- shows the number of frames, otherwise the output may be very big
  show (MP3Structure s) = "MP3Structure{" <> show (VU.length s) <> " frames}"

newtype XingHeader = XingHeader { getXingHeader :: ByteString }

-- | Creates the Xing header for the given MP3. It requires the MP3 frames and
-- the filesize (the assumption is that the total of frame sizes is the filesize
-- of the MP3). The header is created in a MPEG1 Layer 3, 44100 Hz, 48 kb/s,
-- mono frame (156 bytes), which is the smallest 44.1 kHz one that can store the
-- header (137 bytes).
calculateXingHeader :: MP3Structure -> XingHeader
calculateXingHeader mp3@(MP3Structure mp3Frames) =
  XingHeader . BS.toStrict . BSB.toLazyByteString . mconcat $
    [header, sideInfo, xingId, flags, frames, bytes, toc, padding]

  where
    -- TODO use a frame close to what's already in the file?
    header = BSB.word32BE 0b1111_1111__1111_1011__0011_0000__1100_0100
    sideInfo = BSB.byteString $ BS.replicate 17 0
    xingId = if isCBR mp3 then "Info" else "Xing"
    flags = BSB.word32BE 7 -- Frames .|. Bytes .|. TOC
    xingFrame = 1
    frames = BSB.word32BE . fromIntegral $ VU.length mp3Frames + xingFrame
    -- FIXME remove hardcoded values
    xingFrameSize = 156
    -- it's not clear whether the filesize field should include the size of the
    -- ID3v2 tag; I think ffmpeg and audacity don't include it
    bytes = BSB.word32BE $ mp3Size + xingFrameSize
    (toc, mp3Size) = generateTableOfContents mp3
    padding = BSB.byteString $ BS.replicate (fromIntegral xingFrameSize - 137) 0

-- | CBR is when all frames have the same bitrate.
isCBR :: MP3Structure -> Bool
isCBR (MP3Structure frames) = case VU.uncons frames of
  Just (firstFrame, rest) ->
    VU.all ((== fiBitrate firstFrame) . fiBitrate) rest
  Nothing -> True

generateTableOfContents :: MP3Structure -> (BSB.Builder, Word32)
generateTableOfContents (MP3Structure frames) =
  (mconcat $ tocByte <$> percentages, filesize)

  where
    tocByte :: Int -> BSB.Builder
    tocByte prcnt =
      let maybeFrameOffset = fst <$>
            -- TODO avoid searching from the beginning for every percentage
            VU.find ((>= fromIntegral prcnt / 100.0 * duration) . snd) stats
          scaleToByte = floor @Double . (* 255) . (/ fromIntegral filesize) . fromIntegral
          byteValue = maybe 0 scaleToByte maybeFrameOffset
      in BSB.word8 byteValue

    (filesize, duration) = VU.last stats
    -- this generates a vector of tuples, one for each frame, containing:
    -- frame's offset (in bytes) and frame's time offset (in seconds); and the
    -- last, extra value is the total size in bytes and the total duration in
    -- seconds
    stats = VU.scanl'
      (\(!offset, !dur) frame -> (offset + fromIntegral (frameSize frame), dur + frameDuration frame))
      (0 :: Word32, 0)
      frames

    -- exactly 100 entries
    percentages = [0..99]
