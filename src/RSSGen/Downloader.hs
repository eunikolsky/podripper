module RSSGen.Downloader
  ( Bytes
  , URL
  , getFile

  -- * re-exports
  , Manager
  , MonadIO
  , MonadReader
  , MonadThrow
  ) where

import Control.Monad.Catch
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client
import Network.HTTP.Types

type URL = String

type Bytes = BL.ByteString

-- |Downloads a file by the `URL`, returns the response's body on success, or
-- `Nothing` otherwise.
getFile :: (MonadIO m, MonadThrow m, MonadReader Manager m) => URL -> m (Maybe Bytes)
getFile url = do
  manager <- ask
  request <- parseRequest url
  response <- liftIO $ httpLbs request manager
  pure $ if responseStatus response == ok200
    then Just $ responseBody response
    else Nothing
