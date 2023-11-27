module RSSGen.RunUntil
  ( StepResult(..)
  , fromStepResult
  , runUntil
  , toStepResult
  ) where

import Control.Monad.Logger.CallStack
import Control.Monad.Reader
import Data.Text qualified as T
import Data.Time.Clock
import RSSGen.Duration
import RSSGen.MonadTime
import Text.Show.Unicode

-- | The result of a step: either no usable result, or a result of type `a`.
data StepResult a = NoResult | Result !a
  deriving (Eq)

instance Show a => Show (StepResult a) where
  show NoResult = "NoResult"
  show (Result a) = "Result " <> ushow a

-- TODO is it possible to use `Maybe` directly as a `StepResult` (make the
-- latter a typeclass?)  so that we don't need to wrap and unwrap a `Maybe` in
-- a few use cases

toStepResult :: Maybe a -> StepResult a
toStepResult (Just x) = Result x
toStepResult Nothing = NoResult

fromStepResult :: StepResult a -> Maybe a
fromStepResult (Result x) = Just x
fromStepResult NoResult = Nothing

hasResult :: StepResult a -> Bool
hasResult NoResult = False
hasResult (Result _) = True

-- | Runs the given action `f` until it returns a `Result` or until the current
-- time is on/after `endTime`. The function waits for `retryDelay` before
-- each retry. `desc` should be a short action descriptor, which is used for
-- logging.
-- Note that `f` is called at least once, ignoring `endTime` at the beginning.
runUntil :: forall m a. (MonadTime m, MonadLogger m) => T.Text -> RetryDelay -> UTCTime -> m (StepResult a) -> m (StepResult a)
runUntil desc retryDelay endTime f = flip runReaderT desc $ do
  logD ["running until ", show endTime]
  iter

  where
    iter :: ReaderT T.Text m (StepResult a)
    iter = do
      iterNow <- lift getTime
      logD ["now ", show iterNow, " running action"]

      result <- lift f
      now <- lift getTime
      let outOfTime = now >= endTime
          hasResult' = hasResult result
      logD ["now ", show now, ", has result: ", show hasResult']

      if hasResult' || outOfTime
        then pure result
        else do
          logD ["sleeping for ", show retryDelay]
          lift . sleep $ toDuration retryDelay
          iter

logD :: (MonadLogger m, MonadReader T.Text m) => [String] -> m ()
logD xs = do
  -- TODO provide this with the logger itself outside?
  desc <- ask
  let prefix = mconcat ["[", desc, "] "]
  logDebugN . (prefix <>) . T.pack . mconcat $ xs
