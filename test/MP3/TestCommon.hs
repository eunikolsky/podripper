module MP3.TestCommon
  ( allFrameVersions
  ) where

import MP3.MP3

allFrameVersions :: [(SamplingRate, Bitrate, Channel, Padding)]
allFrameVersions =
  [(sr, br, ch, pad) | sr <- allBounded, br <- allBounded, ch <- allBounded, pad <- allBounded]

allBounded :: (Bounded a, Enum a) => [a]
allBounded = [minBound..maxBound]
