module MP3.FrameInfoSpec where

import Control.Monad
import MP3.FrameInfo
import MP3.MP3
import Test.Hspec

spec :: Spec
spec = do
  describe "FrameInfo" $ do
    forM_ allSamplingRates $ \sr ->
      forM_ allBitrates $ \br ->
        it ("preserves information for frame " <> show sr <> ", " <> show br) $ do
          let frame = mkFrameInfo sr br
          fiSamplingRate frame `shouldBe` sr
          fiBitrate frame `shouldBe` br

allSamplingRates :: [SamplingRate]
allSamplingRates = [SR32000Hz, SR44100Hz, SR48000Hz]

allBitrates :: [Bitrate]
allBitrates = enumFromTo minBound maxBound
