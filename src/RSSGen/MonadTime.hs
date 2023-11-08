module RSSGen.MonadTime
  ( Duration
  , MonadTime(..)
  ) where

import Control.Concurrent
import Data.Time.Clock

-- | A duration of time between two `UTCTime`s.
type Duration = NominalDiffTime

-- | Monad that provides access to the time-related functionality, namely
-- getting the current time and sleeping for a duration of time.
class Monad m => MonadTime m where
  getTime :: m UTCTime
  sleep :: Duration -> m ()

instance MonadTime IO where
  getTime = getCurrentTime
  sleep = threadDelay . ceiling . (* microsecondsInSecond) . realToFrac

-- TODO remove duplicate in `Podripper`
microsecondsInSecond :: Double
microsecondsInSecond = 1_000_000
