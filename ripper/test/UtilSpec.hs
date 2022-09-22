{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UtilSpec (spec) where

import Import
import Network.HTTP.Simple (parseRequest_)
import RIO.Writer
import Run
import Test.Hspec

spec :: Spec
spec = do
  describe "ripper" $ do
    context "before first successful recording" $
      it "uses the standard reconnect delay" $ do
        let numActions = 3
            testData = TestData numActions RipNothing

            delay = 10000
            request = parseRequest_ "http://localhost/"
            expectedDelays = replicate numActions delay

            delays = runTestM testData $ ripper request Nothing delay 999

        delays `shouldBe` expectedDelays

    context "after a successful recording" $
      it "uses a small reconnect delay" $ do
        let numActions = 3
            testData = TestData numActions RipRecorded

            smallDelay = 999
            request = parseRequest_ "http://localhost/"
            expectedDelays = replicate numActions smallDelay

            delays = runTestM testData $ ripper request Nothing 10000 smallDelay

        delays `shouldBe` expectedDelays

type NumActions = Int
data TestData = TestData
  { tdNumActions :: NumActions
  , tdRipResult :: RipResult
  }

newtype TestM a = TestM (ReaderT TestData (Writer [Int]) a)
  deriving newtype (Functor, Applicative, Monad, MonadReader TestData, MonadWriter [Int])

runTestM :: TestData -> TestM a -> [Int]
runTestM testData (TestM r) = execWriter $ runReaderT r testData

instance MonadRipper TestM where
  rip _ _ = asks tdRipResult

  delayReconnect delay = tell [delay]

  repeatForever action = do
    numActions <- asks tdNumActions
    replicateM_ numActions action
