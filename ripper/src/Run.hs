{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import qualified Data.Conduit.List as CL
import Network.HTTP.Simple
import qualified RIO.ByteString as S

import Import

run :: RIO App ()
run =
  httpSink "https://httpbin.org/drip?delay=1&duration=1&numbytes=5" $ \response -> do
    logInfo . displayShow . getResponseStatus $ response

    CL.mapM_ (S.hPut stdout)
