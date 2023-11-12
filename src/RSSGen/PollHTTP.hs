module RSSGen.PollHTTP
  ( pollHTTP
  ) where

import Control.Monad.Logger.CallStack
import Data.Time.Clock
import Network.HTTP.Simple
import RSSGen.Database
import RSSGen.Downloader
import RSSGen.MonadTime
import RSSGen.RunUntil
import RSSGen.Types

-- | Wrapper around `getFile` that repeatedly tries to get an updated response
-- from the `url` until it's successful or until `endTime`.
pollHTTP :: (MonadIO m, MonadThrow m, MonadTime m, MonadLogger m)
  => RetryDelay
  -> UTCTime
  -> DBConnection
  -> URL
  -> m (Maybe Bytes)
pollHTTP retryDelay endTime conn url = fromStepResult <$> runUntil "pollHTTP" retryDelay endTime (toStepResult <$> getFile httpBS conn url)
-- TODO add a CLI option for `rssgen` to choose the upstream RSS download:
-- waitForLatest (default); once (old impl); none.

toStepResult :: Maybe Bytes -> StepResult Bytes
toStepResult (Just b) = Result b
toStepResult Nothing = NoResult

fromStepResult :: StepResult Bytes -> Maybe Bytes
fromStepResult (Result b) = Just b
fromStepResult NoResult = Nothing
