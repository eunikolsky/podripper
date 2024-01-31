module MP3.TestCommon
  ( allFrameVersions
  ) where

import MP3.MP3

allFrameVersions :: [(SamplingRate, Bitrate, Channel)]
allFrameVersions =
  [(sr, br, ch) | sr <- allSamplingRates, br <- allBitrates, ch <- allChannels]

allSamplingRates :: [SamplingRate]
allSamplingRates = [SR32000Hz, SR44100Hz, SR48000Hz]

allBitrates :: [Bitrate]
allBitrates = allBounded

allChannels :: [Channel]
allChannels = allBounded

allBounded :: (Bounded a, Enum a) => [a]
allBounded = [minBound..maxBound]
