{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Conduit
import Network.HTTP.Simple

import Import

run :: RIO App ()
run =
  runResourceT $ httpSink "https://httpbin.org/drip?delay=1&duration=1&numbytes=5" $ \response -> do
    logInfo . displayShow . getResponseStatus $ response

    sinkFile "out"
