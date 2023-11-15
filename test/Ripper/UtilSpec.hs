{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ripper.UtilSpec (spec) where

import Data.ByteString.Builder (toLazyByteString)
import Ripper.Import hiding (error)
import Network.HTTP.Simple (parseRequest_)
import RIO.List
import RIO.Partial (fromJust)
import RIO.State
import RIO.Writer
import RSSGen.Duration
import Ripper.Run
import System.IO.Error
import Test.Hspec

spec :: Spec
spec = do
  describe "ripper" $ do
    context "before first successful recording" $
      it "uses the standard reconnect delay" $ do
        let numActions = 3
            testState = TestState (repeat RipNothing) numActions

            delay = RetryDelay $ durationSeconds 3
            delayDiffTime = 3_000_000
            smallDelay = RetryDelay $ durationSeconds 1
            request = parseRequest_ "http://localhost/"
            expectedDelays = replicate numActions delayDiffTime

            delays = runTestM testState $ ripper request Nothing delay smallDelay

        delays `shouldBe` expectedDelays

    context "after a successful recording" $ do
      let smallDelay = RetryDelay $ durationSeconds 1
          smallDelayDiffTime = 1_000_000
          delay = RetryDelay $ durationSeconds 3

      it "uses a small reconnect delay" $ do
        let numActions = 3
            testState = TestState (repeat RipRecorded) numActions

            request = parseRequest_ "http://localhost/"
            expectedDelays = replicate numActions smallDelayDiffTime

            delays = runTestM testState $ ripper request Nothing delay smallDelay

        delays `shouldBe` expectedDelays

      it "uses a small reconnect delay since the first successful recording" $ do
        let numActions = 3
            testState = TestState (RipRecorded : repeat RipNothing) numActions

            request = parseRequest_ "http://localhost/"
            expectedDelays = replicate numActions smallDelayDiffTime

            delays = runTestM testState $ ripper request Nothing delay smallDelay

        delays `shouldBe` expectedDelays

  describe "handleResourceVanished" $ do
    it "catches ResourceVanished IOError" $ do
      let io = liftIO . ioError $ mkIOError resourceVanishedErrorType "Network.Socket.recvBuf" Nothing Nothing
      (_, logOpts) <- logOptionsMemory
      withLogFunc logOpts $ \logFunc -> do
        actual <- flip runReaderT logFunc $ handleResourceVanished io
        actual `shouldBe` RipNothing

    it "doesn't do anything on no exception" $ do
      let io = pure RipRecorded
      (_, logOpts) <- logOptionsMemory
      withLogFunc logOpts $ \logFunc -> do
        actual <- flip runReaderT logFunc $ handleResourceVanished io
        actual `shouldBe` RipRecorded

    it "ignores another IOError" $ do
      let error = mkIOError fullErrorType "" Nothing Nothing
          io = liftIO $ ioError error
      (_, logOpts) <- logOptionsMemory
      withLogFunc logOpts $ \logFunc -> do
        flip runReaderT logFunc (handleResourceVanished io) `shouldThrow` (== error)

    it "logs the exception" $ do
      let io = liftIO . ioError $ mkIOError resourceVanishedErrorType "Network.Socket.recvBuf" Nothing Nothing
      (builderRef, logOpts) <- logOptionsMemory
      _ <- withLogFunc logOpts $ \logFunc ->
        flip runReaderT logFunc $ handleResourceVanished io
      builder <- readIORef builderRef
      toLazyByteString builder `shouldBe` "Network.Socket.recvBuf: resource vanished\n"

type NumActions = Int

data TestState = TestState
  { tsRipResult :: ![RipResult]
  -- ^ this is in a state because we need to change the result during the test,
  -- which is done by removing the head at every step
  , tsNumAction :: !NumActions
  -- ^ the test should terminate when the number reaches zero
  }

type CollectedDelays = [Int]

newtype TestM a = TestM (StateT TestState (Writer CollectedDelays) a)
  deriving newtype (Functor, Applicative, Monad, MonadState TestState, MonadWriter CollectedDelays)

runTestM :: TestState -> TestM a -> CollectedDelays
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
