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
import Data.Maybe
import Data.Vector.Unboxed qualified as VU
import Data.Word
import MP3.FrameInfo
import MP3.Generator
import MP3.MP3
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
calculateXingHeader :: MP3Structure -> (XingHeader, AudioDuration)
calculateXingHeader mp3@(MP3Structure mp3Frames) =
  (XingHeader . generateFrame smallestFrame $ contents, duration)

  where
    -- TODO use a frame close to what's already in the file?
    smallestFrame = mkFrameInfo SR44100Hz BR48kbps Mono
    contents = xingContents <> padding

    xingContents = BS.toStrict . BSB.toLazyByteString . mconcat $
      [sideInfo, xingId, flags, frames, bytes, toc]
    sideInfo = BSB.byteString . zeros $ if isMono smallestFrame then 17 else 32
    xingId = if isCBR mp3 then "Info" else "Xing"
    flags = BSB.word32BE 7 -- Frames .|. Bytes .|. TOC
    xingFrame = 1
    frames = BSB.word32BE . fromIntegral $ VU.length mp3Frames + xingFrame
    xingFrameSize = frameSize smallestFrame
    -- it's not clear whether the filesize field should include the size of the
    -- ID3v2 tag; I think ffmpeg and audacity don't include it
    bytes = BSB.word32BE $ mp3Size + fromIntegral xingFrameSize
    (toc, mp3Size, duration) = generateTableOfContents mp3

    padding = zeros $ fromIntegral xingFrameSize - frameHeaderSize - BS.length xingContents

isMono :: FrameInfo -> Bool
isMono = (== Mono) . fiChannel

zeros :: Int -> ByteString
zeros = (`BS.replicate` 0)

-- | CBR is when all frames have the same bitrate.
isCBR :: MP3Structure -> Bool
isCBR (MP3Structure frames) = case VU.uncons frames of
  Just (firstFrame, rest) ->
    VU.all ((== fiBitrate firstFrame) . fiBitrate) rest
  Nothing -> True

generateTableOfContents :: MP3Structure -> (BSB.Builder, Word32, AudioDuration)
generateTableOfContents (MP3Structure frames) = (tocBytes, filesize, duration)
  where
    tocBytes :: BSB.Builder
    -- this `foldl` avoids the repetitive search from the beginning of the
    -- matching frame for every duration percentage by dropping all the previous
    -- `stats` as the percentage increases because we know that `percentages`
    -- are in the ascending order
    tocBytes = fst $ VU.foldl' appendPercentageByte (mempty, stats) percentages

    appendPercentageByte (bytesAcc, statsLeft) percentage =
      -- this error really shouldn't happen because `stats` always starts with
      -- zeros and `percentages` go from zero, but not to 100
      let index = fromMaybe (error $ "Xing TOC: can't find index for percentage " <> show percentage)
            $ VU.findIndex (forDurationPercentage percentage) statsLeft
      in
        ( bytesAcc <> (BSB.word8 . scaleToByte . frameOffset $ statsLeft VU.! index)
        , (if index > 0 then VU.drop index else id) statsLeft
        )

    (frameOffset, frameTimeOffset) = (fst, snd)

    forDurationPercentage prcnt = (>= fromIntegral prcnt / 100 * duration) . frameTimeOffset
    scaleToByte = floor @Double . (* 255) . (/ fromIntegral filesize) . fromIntegral

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
    percentages = VU.fromList @Word8 [0..99]
