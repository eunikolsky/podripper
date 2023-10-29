module RSSGen.RunUntil
  ( RetryDuration(..)
  , StepResult(..)
  , runUntil
  ) where

import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Text qualified as T
import Data.Time.Clock
import RSSGen.MonadTime

-- | The result of a step: either no usable result, or a result of type `a`.
data StepResult a = NoResult | Result !a
  deriving (Show, Eq)

hasResult :: StepResult a -> Bool
hasResult NoResult = False
hasResult (Result _) = True

-- | Duration of time to sleep for between retries in `runUntil`.
--
-- (Is this separate type really necessary?)
newtype RetryDuration = RetryDuration { toDuration :: Duration }
  deriving newtype Show

-- | Runs the given action `f` until it returns a `Result` or until the current
-- time is on/after `endTime`. The function waits for `retryDuration` before
-- each retry.
-- Note that `f` is called at least once, ignoring `endTime` at the beginning.
runUntil :: (Monad m, MonadTrans t, MonadTime (t m), MonadLogger (t m)) => RetryDuration -> UTCTime -> m (StepResult a) -> t m (StepResult a)
runUntil retryDuration endTime f = do
  logD ["running until ", show endTime]
  iter

  where
    iter = do
      iterNow <- getTime
      logD ["now ", show iterNow, " running action"]

      result <- lift f
      now <- getTime
      let outOfTime = now >= endTime
          hasResult' = hasResult result
      logD ["now ", show now, ", has result: ", show hasResult']

      if hasResult' || outOfTime
        then pure result
        else do
          logD ["sleeping for ", show retryDuration]
          sleep $ toDuration retryDuration
          iter

logD :: MonadLogger m => [String] -> m ()
logD = logDebugN . T.pack . mconcat
