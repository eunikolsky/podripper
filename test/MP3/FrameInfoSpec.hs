module MP3.FrameInfoSpec where

import Control.Monad
import MP3.FrameInfo
import MP3.TestCommon
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
