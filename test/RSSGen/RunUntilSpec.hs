{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RSSGen.RunUntilSpec where

import Control.Monad.Trans.Class
import Control.Monad.Writer.Strict
import Data.Functor
import Data.IORef
import RSSGen.RunUntil
import Test.Hspec

spec :: Spec
spec = describe "runUntil" $ do
  it "returns result on first try" $ do
    actionCallCount <- newIORef @Int 0
    let action = liftIO (modifyIORef' actionCallCount (+ 1)) $> Result ()
    (actual, sleepCount) <- runMockTime (runUntil action)
    callCount <- readIORef actionCallCount

    (actual, callCount, sleepCount) `shouldBe` (Result (), 1, CallCount 0)

  it "retries until there is a result" $ do
    actionCallCount <- newIORef @Int 0
    let action = liftIO $ do
          modifyIORef' actionCallCount (+ 1)
          count <- readIORef actionCallCount
          pure $ if count == 4 then Result () else NoResult
    (actual, sleepCount) <- runMockTime (runUntil action)
    callCount <- readIORef actionCallCount

    (actual, callCount, sleepCount) `shouldBe` (Result (), 4, CallCount 3)

newtype CallCount = CallCount (Sum Int)
  deriving (Semigroup, Monoid, Eq)

instance Show CallCount where
  show (CallCount (Sum n)) = "CallCount " <> show n

newtype MockTimeT m a = MockTimeT (WriterT CallCount m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadWriter CallCount, MonadIO)

instance Monad m => MonadTime (MockTimeT m) where
  sleep = tell . CallCount . Sum $ 1

runMockTime :: MockTimeT m a -> m (a, CallCount)
runMockTime (MockTimeT w) = runWriterT w
