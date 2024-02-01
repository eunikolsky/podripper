module MP3.TestCommon
  ( allFrameVersions
  ) where

import MP3.MP3

allFrameVersions :: [(SamplingRate, Bitrate, Channel, Padding)]
allFrameVersions =
  [(sr, br, ch, pad) | sr <- allSamplingRates, br <- allBitrates, ch <- allChannels, pad <- allPaddings]

allSamplingRates :: [SamplingRate]
allSamplingRates = [SR32000Hz, SR44100Hz, SR48000Hz]

allBitrates :: [Bitrate]
allBitrates = allBounded

allChannels :: [Channel]
allChannels = allBounded

allPaddings :: [Padding]
allPaddings = allBounded

allBounded :: (Bounded a, Enum a) => [a]
allBounded = [minBound..maxBound]
