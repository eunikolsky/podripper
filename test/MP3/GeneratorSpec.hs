module MP3.GeneratorSpec where

import Control.Monad
import Data.Attoparsec.ByteString
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import MP3.FrameInfo
import MP3.Generator
import MP3.MP3
import MP3.Parser
import MP3.TestCommon
import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec = do
  describe "generateFrame" $ do
    forM_ allFrameVersions $ \(sr, br, ch) ->
      it ("generates frame "
          <> show sr <> ", " <> show br <> ", " <> show ch) $ do
        let frame = mkFrameInfo sr br ch
            contentsSize = fromIntegral $ frameContentsSize frame
            contents = BSL.toStrict . BSL.take contentsSize $ BSL.iterate ((`mod` maxBound) . (+ 1)) 0
            parsedFrame = generateFrame frame contents ~> (frameParser <* endOfInput)
        (fInfo <$> parsedFrame) `shouldParse` frame
        (BS.drop frameHeaderSize . getFrameData . fData <$> parsedFrame) `shouldParse` contents

  describe "frameForContentsSize" $ do
    it "returns given frame when its size matches requested size" $
      let frame = mkFrameInfo SR44100Hz BR40kbps Mono -- 130 bytes
      in frameForContentsSize frame 126 `shouldBe` Just frame

    it "returns given frame when its size is larger than requested size" $
      let frame = mkFrameInfo SR44100Hz BR48kbps Mono -- 156 bytes
      in frameForContentsSize frame 126 `shouldBe` Just frame

    it "returns Nothing for big size" $
      -- max frame for 44.1 kHz is 1044 bytes
      frameForContentsSize (mkFrameInfo SR44100Hz BR40kbps Mono) 1041 `shouldBe` Nothing
