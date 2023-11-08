module RSSGen.PollHTTP
  ( pollHTTP
  ) where

import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Time.Clock
import Network.HTTP.Simple
import RSSGen.Database
import RSSGen.Downloader
import RSSGen.MonadTime
import RSSGen.RunUntil

-- | Wrapper around `getFile` that repeatedly tries to get an updated response
-- from the `url` until it's successful or until `endTime`.
pollHTTP :: (MonadIO m, MonadThrow m, MonadTrans t, MonadTime (t m), MonadLogger (t m))
  => RetryDuration
  -> UTCTime
  -> Connection
  -- ^ database connection
  -> URL
  -> t m (Maybe Bytes)
pollHTTP retryDuration endTime conn url = fromStepResult <$> runUntil retryDuration endTime (toStepResult <$> getFile httpBS conn url)
-- TODO add a CLI option for `rssgen` to choose the upstream RSS download:
-- waitForLatest (default); once (old impl); none.

toStepResult :: Maybe Bytes -> StepResult Bytes
toStepResult (Just b) = Result b
toStepResult Nothing = NoResult

fromStepResult :: StepResult Bytes -> Maybe Bytes
fromStepResult (Result b) = Just b
fromStepResult NoResult = Nothing
