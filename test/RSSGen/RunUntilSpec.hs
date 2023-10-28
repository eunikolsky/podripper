module RSSGen.RunUntilSpec where

import Control.Monad.ST
import Data.Functor
import Data.Functor.Identity
import Data.STRef
import RSSGen.RunUntil
import Test.Hspec

spec :: Spec
spec = describe "runUntil" $ do
  it "returns result without waiting" $ do
    let action = pure $ Result ()
    runIdentity (runUntil action) `shouldBe` Result ()

  it "retries once if no result" $ do
    let (actual, callCount) = runST $ do
          actionCallCount <- newSTRef @Int 0
          let action = modifySTRef actionCallCount (+ 1) $> NoResult @()
          (,) <$> runUntil action <*> readSTRef actionCallCount

    (actual, callCount) `shouldBe` (NoResult, 2)
