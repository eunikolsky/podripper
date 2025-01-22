module RSSGen.PollHTTP
  ( httpExceptionHandler
  , pollHTTP
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Data.Functor
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
  => UserAgent
  -> RetryDelay
  -> UTCTime
  -> DBConnection
  -> URL
  -> m (Maybe Bytes)
pollHTTP userAgent retryDelay endTime conn url = fromStepResult <$>
  runUntil "pollHTTP" retryDelay endTime
    (handleHTTPExceptions $ toStepResult <$> getFile userAgent httpBS conn url)

  where
    handleHTTPExceptions :: (MonadUnliftIO m, MonadLogger m) => m (StepResult Bytes) -> m (StepResult Bytes)
    handleHTTPExceptions = handle $ \e -> httpExceptionHandler e $> NoResult
-- TODO add a CLI option for `rssgen` to choose the upstream RSS download:
-- waitForLatest (default); once (old impl); none.

-- | Catch and log an HTTP exception if it's thrown (e.g. `ConnectionTimeout`),
-- so that it doesn't terminate the polling.
httpExceptionHandler :: MonadLogger m => HttpException -> m ()
httpExceptionHandler e =
  logDebugN . T.pack $ case e of
    HttpExceptionRequest _req content -> show content
    InvalidUrlException _ _ -> show e
