module RSSGen.Downloader
  ( getFile

  -- * re-exports
  , Bytes
  , Manager
  , MonadIO
  , MonadReader
  , MonadThrow
  , URL
  ) where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.ByteString qualified as B
import Network.HTTP.Client
import Network.HTTP.Types
import RSSGen.DownloaderTypes

-- |Downloads a file by the `URL`, returns the response's body on success, or
-- `Nothing` otherwise.
getFile :: (MonadIO m, MonadThrow m, MonadReader Manager m) => URL -> m (Maybe Bytes)
getFile url = do
  manager <- ask
  request <- parseRequest url
  response <- liftIO $ httpLbs request manager
  pure $ if responseStatus response == ok200
    then Just . B.toStrict $ responseBody response
    else Nothing
