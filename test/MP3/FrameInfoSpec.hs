module MP3.FrameInfoSpec where

import Control.Monad
import MP3.FrameInfo
import MP3.MP3
import Test.Hspec

spec :: Spec
spec = do
  describe "FrameInfo" $ do
    forM_ allFrameVersions $ \(sr, br, ch) ->
      it ("preserves information for frame "
          <> show sr <> ", " <> show br <> ", " <> show ch) $ do
        let frame = mkFrameInfo sr br ch
        fiSamplingRate frame `shouldBe` sr
        fiBitrate frame `shouldBe` br
        fiChannel frame `shouldBe` ch

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
