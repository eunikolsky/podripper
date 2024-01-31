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
        it "preserves frame information" $ do
          let frame = mkFrameInfo sr br
          fiSamplingRate frame `shouldBe` sr
          fiBitrate frame `shouldBe` br

allSamplingRates :: [SamplingRate]
allSamplingRates = [MPEG2SR SR16000Hz, MPEG2SR SR22050Hz, MPEG2SR SR24000Hz, MPEG1SR SR32000Hz, MPEG1SR SR44100Hz, MPEG1SR SR48000Hz]

allBitrates :: [Bitrate]
allBitrates = enumFromTo minBound maxBound
