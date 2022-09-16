{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Conduit
import Network.HTTP.Conduit (HttpExceptionContent(..))
import Network.HTTP.Simple
import qualified RIO.Text as T
import RIO.Time
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Import

ripTimeout :: Int
ripTimeout = 4 * seconds
  where seconds = 1_000_000

run :: RIO App ()
run = do
  options <- asks appOptions
  let maybeOutputDir = optionsOutputDirectory options
  for_ maybeOutputDir ensureDirectory

  request <- parseRequestThrow . T.unpack . optionsStreamURL $ options
  runResourceT
    . handle httpExceptionHandler
    . void . timeout ripTimeout
    . httpSink request
    $ \response -> do
    logInfo . displayShow . getResponseStatus $ response

    {-
     - I want to include the time at which we start receiving the response body
     - (ideally, the body's first byte, not the header) in the filename;
     - in this block, we have the `response` and are ready to stream the body,
     - so this time should be good enough
     -}

    filename <- liftIO getFilename
    sinkFile $ maybe filename (</> filename) maybeOutputDir

  where
    httpExceptionHandler :: (MonadIO m, MonadReader env m, HasLogFunc env) => HttpException -> m ()
    httpExceptionHandler e = logError $ case e of
      -- for http exceptions, we don't print the request, only the exception details
      HttpExceptionRequest _ content -> case content of
        StatusCodeException response _ -> "Unsuccessful response: " <> displayShow (getResponseStatus response)
        _ -> displayShow content
      -- for invalid url exceptions, we print it as is
      _ -> displayShow e

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
