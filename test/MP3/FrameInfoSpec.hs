module MP3.FrameInfoSpec where

import Control.Monad
import MP3.FrameInfo
import MP3.TestCommon
import Test.Hspec

spec :: Spec
spec = do
  describe "FrameInfo" $ do
    forM_ allFrameVersions $ \fr@(sr, br, ch, pad) ->
      it ("preserves information for frame " <> show fr) $ do
        let frame = mkFrameInfo sr br ch pad
        fiSamplingRate frame `shouldBe` sr
        fiBitrate frame `shouldBe` br
        fiChannel frame `shouldBe` ch
        fiPadding frame `shouldBe` pad
