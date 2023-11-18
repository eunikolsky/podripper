module RSSGen.PollHTTP
  ( pollHTTP
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Data.Text qualified as T
import Data.Time.Clock
import Network.HTTP.Simple
import RSSGen.Database
import RSSGen.Downloader
import RSSGen.Duration
import RSSGen.MonadTime
import RSSGen.RunUntil
import UnliftIO.Exception

-- | Wrapper around `getFile` that repeatedly tries to get an updated response
-- from the `url` until it's successful or until `endTime`.
pollHTTP :: (MonadUnliftIO m, MonadThrow m, MonadTime m, MonadLogger m)
  => RetryDelay
  -> UTCTime
  -> DBConnection
  -> URL
  -> m (Maybe Bytes)
pollHTTP retryDelay endTime conn url = fromStepResult <$>
  runUntil "pollHTTP" retryDelay endTime
    (handle httpExceptionHandler $ toStepResult <$> getFile httpBS conn url)
-- TODO add a CLI option for `rssgen` to choose the upstream RSS download:
-- waitForLatest (default); once (old impl); none.

-- | Catch and log an HTTP exception if it's thrown (e.g. `ConnectionTimeout`),
-- so that it doesn't terminate the polling.
httpExceptionHandler :: MonadLogger m => HttpException -> m (StepResult a)
httpExceptionHandler e = do
  logDebugN . T.pack $ case e of
    HttpExceptionRequest _req content -> show content
    InvalidUrlException _ _ -> show e

  pure NoResult
