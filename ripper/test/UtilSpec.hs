{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UtilSpec (spec) where

import Import hiding (error)
import Network.HTTP.Simple (parseRequest_)
import RIO.List
import RIO.Partial (fromJust)
import RIO.State
import RIO.Writer
import Run
import System.IO.Error
import Test.Hspec

spec :: Spec
spec = do
  describe "ripper" $ do
    context "before first successful recording" $
      it "uses the standard reconnect delay" $ do
        let numActions = 3
            testState = TestState (repeat RipNothing) numActions

            delay = 10000
            request = parseRequest_ "http://localhost/"
            expectedDelays = replicate numActions delay

            delays = runTestM testState $ ripper request Nothing delay 999

        delays `shouldBe` expectedDelays

    context "after a successful recording" $ do
      it "uses a small reconnect delay" $ do
        let numActions = 3
            testState = TestState (repeat RipRecorded) numActions

            smallDelay = 999
            request = parseRequest_ "http://localhost/"
            expectedDelays = replicate numActions smallDelay

            delays = runTestM testState $ ripper request Nothing 10000 smallDelay

        delays `shouldBe` expectedDelays

      it "uses a small reconnect delay since the first successful recording" $ do
        let numActions = 3
            testState = TestState (RipRecorded : repeat RipNothing) numActions

            smallDelay = 999
            request = parseRequest_ "http://localhost/"
            expectedDelays = replicate numActions smallDelay

            delays = runTestM testState $ ripper request Nothing 10000 smallDelay

        delays `shouldBe` expectedDelays

  describe "handleResourceVanished" $ do
    it "catches ResourceVanished IOError" $ do
      let io = ioError $ mkIOError resourceVanishedErrorType "Network.Socket.recvBuf" Nothing Nothing
      actual <- handleResourceVanished io
      actual `shouldBe` RipNothing

    it "doesn't do anything on no exception" $ do
      let io = pure RipRecorded
      actual <- handleResourceVanished io
      actual `shouldBe` RipRecorded

    it "ignores another IOError" $ do
      let error = mkIOError fullErrorType "" Nothing Nothing
          io = ioError error
      handleResourceVanished io `shouldThrow` (== error)

type NumActions = Int

data TestState = TestState
  { tsRipResult :: ![RipResult]
  -- ^ this is in a state because we need to change the result during the test,
  -- which is done by removing the head at every step
  , tsNumAction :: !NumActions
  -- ^ the test should terminate when the number reaches zero
  }

newtype TestM a = TestM (StateT TestState (Writer [Int]) a)
  deriving newtype (Functor, Applicative, Monad, MonadState TestState, MonadWriter [Int])

runTestM :: TestState -> TestM a -> [Int]
runTestM testState (TestM r) = execWriter $ evalStateT r testState

instance MonadRipper TestM where
  rip _ _ = do
    s <- get
    let (head, tail) = fromJust . uncons $ tsRipResult s
    put $ s { tsRipResult = tail }
    pure head

  delayReconnect delay = tell [delay]

  shouldRepeat = do
    s <- get
    let iteration = tsNumAction s - 1
    put $ s { tsNumAction = iteration }

    pure $ iteration > 0
