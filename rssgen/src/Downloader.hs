{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Downloader where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL

type URL = String

type Bytes = BL.ByteString

-- |The API to download files via HTTP(S).
class Monad m => MonadDownload m where
  -- |Downloads a file by the @URL@. Returns @Nothing@ for an error response.
  getFile :: URL -> m (Maybe Bytes)


-- |A fake downloader that always returns an XML with the commented URL.
newtype FakeDownloadT m a = FakeDownloadT { runFakeDownloadT :: m a }
  deriving (Functor, Applicative, Monad)

instance Monad m => MonadDownload (FakeDownloadT m) where
  getFile url = pure . Just $ "<!-- " <> CL.pack url <> " -->"
