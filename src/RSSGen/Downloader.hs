module RSSGen.Downloader
  ( Bytes
  , DownloadResponse(..)
  , HTTPClientDownloadT(..)
  , MonadDownload(..)
  , URL
  , successfulBody
  ) where

import Control.Monad.Catch
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client
import Network.HTTP.Types

type URL = String

type Bytes = BL.ByteString

-- | Contains the fields from `Network.HTTP.Client.Response` that are important
-- for `MonadDownload` implementations. Creating values of this type doesn't
-- require importing `Network.HTTP.Client.Internal`.
data DownloadResponse = DownloadResponse
  { drStatus :: !Status
  , drHeaders :: !ResponseHeaders
  , drBody :: !Bytes
  }

fromResponse :: Response Bytes -> DownloadResponse
fromResponse r = DownloadResponse
  { drStatus = responseStatus r
  , drHeaders = responseHeaders r
  , drBody = responseBody r
  }

-- |The API to download files via HTTP(S).
class Monad m => MonadDownload m where
  -- |Downloads a file by the @URL@.
  getFile :: URL -> m DownloadResponse


-- |A downloader that uses `Network.HTTP.Client`.
newtype HTTPClientDownloadT m a = HTTPClientDownloadT { runHTTPClientDownloadT :: ReaderT Manager m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Manager, MonadThrow)

instance (MonadIO m, MonadThrow m) => MonadDownload (HTTPClientDownloadT m) where
  getFile url = do
    manager <- ask
    request <- parseRequest url
    response <- liftIO $ httpLbs request manager
    pure $ fromResponse response

-- | Returns the `response`'s body if the download is successful, and `Nothing`
-- otherwise.
successfulBody :: DownloadResponse -> Maybe Bytes
successfulBody response =
  if drStatus response == ok200
    then Just $ drBody response
    else Nothing
