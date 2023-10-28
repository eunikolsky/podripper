module RSSGen.RunUntil
  ( StepResult(..)
  , runUntil
  ) where

data StepResult a = NoResult | Result a
  deriving (Show, Eq)

hasResult :: StepResult a -> Bool
hasResult NoResult = False
hasResult (Result _) = True

runUntil :: Monad m => m (StepResult a) -> m (StepResult a)
runUntil f = do
  result <- f
  if not (hasResult result)
    then f
    else pure result
