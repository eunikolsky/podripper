module RSSGen.MonadTime
  ( MonadTime(..)
  ) where

import Control.Concurrent
import Control.Monad.Logger.CallStack
import Control.Monad.Trans
import Data.Time.Clock
import RSSGen.Duration

-- | Monad that provides access to the time-related functionality, namely
-- getting the current time and sleeping for a duration of time.
class Monad m => MonadTime m where
  getTime :: m UTCTime
  sleep :: Duration -> m ()

instance MonadTime IO where
  getTime = getCurrentTime
  sleep = threadDelay . toMicroseconds

instance MonadTime m => MonadTime (LoggingT m) where
  getTime = lift getTime
  sleep = lift . sleep
