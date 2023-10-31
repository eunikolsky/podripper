module RSSGen.Downloader
  ( Bytes
  , HTTPClientDownloadT(..)
  , MonadDownload(..)
  , URL
  ) where

import Control.Monad.Catch
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client
import Network.HTTP.Types

type URL = String

type Bytes = BL.ByteString

-- |The API to download files via HTTP(S).
class Monad m => MonadDownload m where
  -- |Downloads a file by the @URL@.
  getFile :: URL -> m (Maybe Bytes)


-- |A downloader that uses `Network.HTTP.Client`.
newtype HTTPClientDownloadT m a = HTTPClientDownloadT { runHTTPClientDownloadT :: ReaderT Manager m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Manager, MonadThrow)

instance (MonadIO m, MonadThrow m) => MonadDownload (HTTPClientDownloadT m) where
  getFile url = do
    manager <- ask
    request <- parseRequest url
    response <- liftIO $ httpLbs request manager
    pure $ if responseStatus response == ok200
      then Just $ responseBody response
      else Nothing
