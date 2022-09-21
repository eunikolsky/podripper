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
  describe "ripper" $
    context "before first successful recording" $
      it "uses the standard reconnect delay" $ do
        let numActions = 3

            delay = 10000
            request = parseRequest_ "http://localhost/"
            expectedDelays = replicate numActions delay

            delays = runTestM numActions $ ripper request Nothing delay

        delays `shouldBe` expectedDelays

type NumActions = Int
newtype TestM a = TestM (ReaderT NumActions (Writer [Int]) a)
  deriving newtype (Functor, Applicative, Monad, MonadReader Int, MonadWriter [Int])

runTestM :: NumActions -> TestM a -> [Int]
runTestM numActions (TestM r) = execWriter $ runReaderT r numActions

instance MonadRipper TestM where
  rip _ _ = pure ()
  delayReconnect delay = tell [delay]
  repeatForever action = do
    numActions <- ask
    replicateM_ numActions action
