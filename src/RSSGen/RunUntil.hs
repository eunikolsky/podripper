module RSSGen.RunUntil
  ( StepResult(..)
  , runUntil
  ) where

data StepResult a = NoResult | Result a
  deriving (Show, Eq)

runUntil :: Monad m => m a -> m a
runUntil f = f >> f
