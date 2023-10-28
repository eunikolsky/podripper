module RSSGen.RunUntil
  ( StepResult(..)
  , runUntil
  ) where

data StepResult a = NoResult | Result a
  deriving (Show, Eq)

runUntil :: a -> a
runUntil f = f
