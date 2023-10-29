module RSSGen.RunUntil
  ( RetryDuration(..)
  , StepResult(..)
  , runUntil
  ) where

import Control.Monad.Trans.Class
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

-- | Runs the given action `f` until it returns a `Result` or until the current
-- time is on/after `endTime`. The function waits for `retryDuration` before
-- each retry.
-- Note that `f` is called at least once, ignoring `endTime` at the beginning.
runUntil :: (Monad m, MonadTrans t, MonadTime (t m)) => RetryDuration -> UTCTime -> m (StepResult a) -> t m (StepResult a)
runUntil retryDuration endTime f = do
  result <- lift f
  now <- getTime
  let outOfTime = now >= endTime
  if hasResult result || outOfTime
    then pure result
    else sleep (toDuration retryDuration) >> runUntil retryDuration endTime f
