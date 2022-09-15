{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Conduit
import Network.HTTP.Simple
import RIO.Time

import Import

run :: RIO App ()
run =
  runResourceT $ httpSink "https://httpbin.org/drip?delay=1&duration=1&numbytes=5" $ \response -> do
    logInfo . displayShow . getResponseStatus $ response

    {-
     - I want to include the time at which we start receiving the response body
     - (ideally, the body's first byte, not the header) in the filename;
     - in this block, we have the `response` and are ready to stream the body,
     - so this time should be good enough
     -}

    filename <- liftIO getFilename
    sinkFile filename

-- | Returns a rip filename that follows the `streamripper`'s pattern:
-- `sr_program_YYYY_mm_dd_HH_MM_SS.mp3` in the current timezone.
--
-- https://en.wikipedia.org/wiki/Streamripper
getFilename :: IO FilePath
getFilename = do
  time <- getCurrentLocalTime
  let timeString = formatTime defaultTimeLocale "%_Y_%m_%d_%H_%M_%S" time
  pure $ "sr_program_" <> timeString <> ".mp3"

  where
    getCurrentLocalTime :: MonadIO m => m LocalTime
    getCurrentLocalTime = zonedTimeToLocalTime <$> getZonedTime
