module RSSGen.RunUntil
  ( Duration
  , MonadTime(..)
  , RetryDuration(..)
  , StepResult(..)
  , runUntil
  ) where

import Data.Time.Clock

type Duration = NominalDiffTime

class Monad m => MonadTime m where
  getTime :: m UTCTime
  sleep :: Duration -> m ()

data StepResult a = NoResult | Result a
  deriving (Show, Eq)

hasResult :: StepResult a -> Bool
hasResult NoResult = False
hasResult (Result _) = True

newtype RetryDuration = RetryDuration { toDuration :: Duration }

runUntil :: MonadTime m => RetryDuration -> UTCTime -> m (StepResult a) -> m (StepResult a)
runUntil retryDuration endTime f = do
  result <- f
  now <- getTime
  let outOfTime = now >= endTime
  if hasResult result || outOfTime
    then pure result
    else sleep (toDuration retryDuration) >> runUntil retryDuration endTime f
