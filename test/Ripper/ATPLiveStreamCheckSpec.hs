{-# LANGUAGE QuasiQuotes #-}

module Ripper.ATPLiveStreamCheckSpec where

import Ripper.ATPLiveStreamCheck
import Ripper.Types
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "extractURL" $ do
    it "returns `src` of audio > source" $
      extractURL [r|
        <div><p>hi</p><audio controls><source src="https://example.org/" type="audio/mpeg" /></audio></div>
      |] `shouldBe` Just (StreamURL $ URL "https://example.org/")

    it "returns `src` of audio" $
      extractURL [r|
        <div><p>hi</p><audio controls="controls" src="https://example.net/"></audio></div>
      |] `shouldBe` Just (StreamURL $ URL "https://example.net/")
