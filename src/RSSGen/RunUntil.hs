module RSSGen.RunUntil
  ( MonadTime(..)
  , StepResult(..)
  , runUntil
  ) where

import Data.Time.Clock

class Monad m => MonadTime m where
  getTime :: m UTCTime
  sleep :: m ()

data StepResult a = NoResult | Result a
  deriving (Show, Eq)

hasResult :: StepResult a -> Bool
hasResult NoResult = False
hasResult (Result _) = True

runUntil :: MonadTime m => UTCTime -> m (StepResult a) -> m (StepResult a)
runUntil endTime f = do
  result <- f
  if hasResult result
    then pure result
    else do
      now <- getTime
      let outOfTime = now >= endTime
      if outOfTime
        then pure result
        else sleep >> runUntil endTime f
