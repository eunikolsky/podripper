module RSSGen.RunUntil
  ( MonadTime(..)
  , StepResult(..)
  , runUntil
  ) where

class Monad m => MonadTime m where
  sleep :: m ()

data StepResult a = NoResult | Result a
  deriving (Show, Eq)

hasResult :: StepResult a -> Bool
hasResult NoResult = False
hasResult (Result _) = True

runUntil :: MonadTime m => m (StepResult a) -> m (StepResult a)
runUntil f = do
  result <- f
  if not (hasResult result)
    then sleep >> f
    else pure result
