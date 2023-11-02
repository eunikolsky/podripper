module RSSGen.Downloader
  ( getFile

  -- * re-exports
  , Bytes
  , MonadIO
  , MonadReader
  , MonadThrow
  , URL
  ) where

import Control.Monad.Catch
import Control.Monad.Reader
import Network.HTTP.Client (Response(..))
import Network.HTTP.Simple
import Network.HTTP.Types
import RSSGen.DownloaderTypes

-- |Downloads a file by the `URL`, returns the response's body on success, or
-- `Nothing` otherwise.
getFile :: (MonadIO m, MonadThrow m) => URL -> m (Maybe Bytes)
getFile url = do
  request <- parseRequest url
  response <- liftIO $ httpBS request
  pure $ if responseStatus response == ok200
    then Just $ responseBody response
    else Nothing
