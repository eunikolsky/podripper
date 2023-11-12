{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RSSGen.RunUntilSpec where

import Control.Monad.Logger.CallStack
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Functor
import Data.IORef
import Data.Monoid
import Data.Time
import RSSGen.Duration
import RSSGen.MonadTime
import RSSGen.RunUntil
import RSSGen.Types
import Test.Hspec

spec :: Spec
spec = describe "runUntil" $ do
  it "returns result on first try" $ do
    actionCallCount <- newIORef @Int 0
    let action = liftIO $ modifyIORef' actionCallCount (+ 1) $> Result ()
    (actual, sleepCount) <- runNoLoggingT $ runMockTime startTime (runUntil "" retryDelay endTime action)
    callCount <- readIORef actionCallCount

    (actual, callCount, sleepCount) `shouldBe` (Result (), 1, CallCount 0)

  it "retries until there is a result" $ do
    actionCallCount <- newIORef @Int 0
    let action = liftIO $ do
          modifyIORef' actionCallCount (+ 1)
          count <- readIORef actionCallCount
          pure $ if count == 4 then Result () else NoResult
    (actual, sleepCount) <- runNoLoggingT $ runMockTime startTime (runUntil "" retryDelay endTime action)
    callCount <- readIORef actionCallCount

    (actual, callCount, sleepCount) `shouldBe` (Result (), 4, CallCount 3)

  it "stops at end time if there is no result" $ do
    actionCallCount <- newIORef @Int 0
    let action = liftIO $ do
          modifyIORef' actionCallCount (+ 1)
          count <- readIORef actionCallCount
          pure $
            -- this is to prevent a possible infinite loop
            if count == 100 then error "should not have been called" else
            NoResult @()
    (actual, sleepCount) <- runNoLoggingT . runMockTime startTime $ runUntil "" retryDelay endTime action
    callCount <- readIORef actionCallCount

    (actual, callCount, sleepCount) `shouldBe` (NoResult, 8, CallCount 7)

startTime, endTime :: UTCTime
startTime = testDay 0 1 0
endTime = testDay 0 8 0

retryDelay :: RetryDelay
retryDelay = RetryDelay $ Duration 60

testDay :: Int -> Int -> Int -> UTCTime
testDay h m s = UTCTime (fromGregorian 2023 01 01) (secondsToDiffTime . fromIntegral $ s + (m * 60) + (h * 3600))

newtype CallCount = CallCount (Sum Int)
  deriving (Semigroup, Monoid, Eq)

instance Show CallCount where
  show (CallCount (Sum n)) = "CallCount " <> show n

-- | A mocking implementation of `MonadTime` for tests. It was created to avoid
-- using real delays for faster unit tests, at the expense of having (probably)
-- harder to understand tests and not so precise time semantics (e.g. calling
-- `getTime` many times in a row will return the same time). The tracked current
-- time is increased by the given `sleep` durations; `CallCount` tracks the
-- number of `sleep` calls.
newtype MockTimeT m a = MockTimeT (StateT (UTCTime, CallCount) m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadState (UTCTime, CallCount), MonadIO, MonadLogger)

instance Monad m => MonadTime (MockTimeT m) where
  getTime = gets fst
  sleep duration = modify' $ \(time, callCount) ->
    (addUTCTime (toNominalDiffTime duration) time, callCount <> (CallCount . Sum $ 1))

runMockTime :: Monad m => UTCTime -> MockTimeT m a -> m (a, CallCount)
runMockTime time (MockTimeT w) = second snd <$> runStateT w (time, CallCount 0)
