module RSSGen.RunUntilSpec where

import RSSGen.RunUntil
import Test.Hspec

spec :: Spec
spec = describe "runUntil" $
  it "returns success without waiting" $ do
    let action = Result ()
    runUntil action `shouldBe` Result ()
