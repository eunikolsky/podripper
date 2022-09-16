{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Conduit
import Network.HTTP.Simple
import qualified RIO.Text as T
import RIO.Time
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Import

run :: RIO App ()
run = do
  options <- asks appOptions
  let maybeOutputDir = optionsOutputDirectory options
  for_ maybeOutputDir ensureDirectory

  request <- parseRequest . T.unpack . optionsStreamURL $ options
  runResourceT $ httpSink request $ \response -> do
    logInfo . displayShow . getResponseStatus $ response

    {-
     - I want to include the time at which we start receiving the response body
     - (ideally, the body's first byte, not the header) in the filename;
     - in this block, we have the `response` and are ready to stream the body,
     - so this time should be good enough
     -}

    filename <- liftIO getFilename
    sinkFile $ maybe filename (</> filename) maybeOutputDir

ensureDirectory :: MonadIO m => FilePath -> m ()
ensureDirectory = liftIO . createDirectoryIfMissing createParents
  where createParents = True

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
