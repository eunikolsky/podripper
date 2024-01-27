{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Ripper.RunSpec (spec) where

import Data.ByteString.Builder (toLazyByteString)
import Data.List (singleton)
import Data.Time.TZTime
import Data.Time.TZTime.QQ
import MP3.Xing
import Ripper.Import hiding (error)
import RIO.List
import RIO.Partial (fromJust)
import RIO.State
import RIO.Writer
import RSSGen.Duration
import Ripper.RipperDelay
import Ripper.Run
import System.IO.Error
import Test.Hspec

spec :: Spec
spec = do
  describe "ripper" $ do
    context "before first successful recording" $
      it "uses a no-recording delay" $ do
        let numActions = 3
            delay = RetryDelay $ durationSeconds 3
            testState = TestState (repeat RipNothing) numActions delay now

            expectedArgs = (replicate numActions delay, replicate numActions Nothing)

            args = runTestM testState $ ripper "" Nothing Nothing mempty testURL

        args `shouldBe` expectedArgs

    context "after a successful recording" $ do
      let delay = RetryDelay $ durationSeconds 3
          ripEndTime = [tz|2023-12-31 22:59:00 [UTC]|]

      it "uses an after-recording delay" $ do
        let numActions = 1
            testState = TestState (repeat $ RipRecorded emptyRip ripEndTime) numActions delay now

            expectedArgs = ([delay], [Just ripEndTime])

            args = runTestM testState $ ripper "" Nothing Nothing mempty testURL

        args `shouldBe` expectedArgs

      it "uses an after-recording delay since the first successful recording" $ do
        let numActions = 3
            testState = TestState (RipRecorded emptyRip ripEndTime : repeat RipNothing) numActions delay now

            expectedArgs = (replicate numActions delay, replicate numActions $ Just ripEndTime)

            args = runTestM testState $ ripper "" Nothing Nothing mempty testURL

        args `shouldBe` expectedArgs

  describe "handleResourceVanished" $ do
    it "catches ResourceVanished IOError" $ do
      let io = liftIO . ioError $ mkIOError resourceVanishedErrorType "Network.Socket.recvBuf" Nothing Nothing
      (_, logOpts) <- logOptionsMemory
      withLogFunc logOpts $ \logFunc -> do
        actual <- flip runReaderT logFunc $ handleResourceVanished io
        actual `shouldBe` RipNothing

    it "doesn't do anything on no exception" $ do
      let result = RipRecorded emptyRip now
          io = pure result
      (_, logOpts) <- logOptionsMemory
      withLogFunc logOpts $ \logFunc -> do
        actual <- flip runReaderT logFunc $ handleResourceVanished io
        actual `shouldBe` result

    it "ignores another IOError" $ do
      let error = mkIOError fullErrorType "" Nothing Nothing
          io = liftIO $ ioError error
      (_, logOpts) <- logOptionsMemory
      withLogFunc logOpts $ \logFunc -> do
        runReaderT (handleResourceVanished io) logFunc `shouldThrow` (== error)

    it "logs the exception" $ do
      let io = liftIO . ioError $ mkIOError resourceVanishedErrorType "Network.Socket.recvBuf" Nothing Nothing
      (builderRef, logOpts) <- logOptionsMemory
      _ <- withLogFunc logOpts $ \logFunc ->
        flip runReaderT logFunc $ handleResourceVanished io
      builder <- readIORef builderRef
      toLazyByteString builder `shouldBe` "Network.Socket.recvBuf: resource vanished\n"

emptyRip :: SuccessfulRip
emptyRip = SuccessfulRip "" (MP3Structure mempty) Nothing

testURL :: StreamConfig
testURL = SimpleURL . StreamURL $ URL "http://localhost/"

now :: TZTime
now = [tz|2023-12-31 23:00:00 [UTC]|]

type NumActions = Int

data TestState = TestState
  { tsRipResult :: ![RipResult]
  -- ^ this is in a state because we need to change the result during the test,
  -- which is done by removing the head at every step
  , tsNumAction :: !NumActions
  -- ^ the test should terminate when the number reaches zero
  , tsRipDelay :: !RetryDelay
  -- ^ the ripper delay to return in test
  , tsNow :: !Now
  -- ^ the emulated `now` to return in test
  }

-- | Collects the corresponding arguments to `delayReconnect` and `getRipDelay`
-- in the calling order to verify them later.
type CollectedArgs = ([RetryDelay], [Maybe RipEndTime])

newtype TestM a = TestM (StateT TestState (Writer CollectedArgs) a)
  deriving newtype (Functor, Applicative, Monad, MonadState TestState, MonadWriter CollectedArgs)

runTestM :: TestState -> TestM a -> CollectedArgs
runTestM testState (TestM r) = execWriter $ evalStateT r testState

instance MonadRipper TestM where
  rip _ _ _ = do
    s <- get
    let (head, tail) = fromJust . uncons $ tsRipResult s
    put $ s { tsRipResult = tail }
    pure head

  checkLiveStream _name = pure . Just

  getRipDelay _ ripEndTime _ = tellRipEndTime ripEndTime >> gets tsRipDelay

  getTime = gets tsNow

  delayReconnect delay = tellRetryDelay delay

  shouldRepeat = do
    s <- get
    let iteration = tsNumAction s - 1
    put $ s { tsNumAction = iteration }

    pure $ iteration > 0

  notifyRip = const $ pure ()

tellRetryDelay :: RetryDelay -> TestM ()
tellRetryDelay = tell . (, []) . singleton

tellRipEndTime :: Maybe RipEndTime -> TestM ()
tellRipEndTime = tell . ([], ) . singleton
