{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run
  ( MonadRipper(..)
  , RipResult(..)
  , run, ripper
  ) where

import Conduit
import Data.Monoid (Any(..))
import Network.HTTP.Conduit (HttpExceptionContent(..))
import Network.HTTP.Simple
import RIO.Directory (createDirectoryIfMissing)
import RIO.FilePath ((</>))
import qualified RIO.Text as T
import RIO.Time

import Import

run :: RIO App ()
run = do
  options <- asks appOptions
  let maybeOutputDir = optionsOutputDirectory options
  for_ maybeOutputDir ensureDirectory

  let ripTimeout = secondsToTimeout $ optionsRipLengthSeconds options
      reconnectDelay = secondsToTimeout $ optionsReconnectDelay options
      smallReconnectDelay = secondsToTimeout $ optionsSmallReconnectDelay options

  request <- parseRequestThrow . T.unpack . optionsStreamURL $ options
  void . timeout ripTimeout
    $ ripper request maybeOutputDir reconnectDelay smallReconnectDelay

-- | The result of a single rip call. The information conveyed by this type
-- is whether the call has received and saved any data.
data RipResult = RipRecorded | RipNothing
  deriving Eq

class Monad m => MonadRipper m where
  rip :: Request -> Maybe FilePath -> m RipResult
  delayReconnect :: Int -> m ()
  shouldRepeat :: m Bool

instance HasLogFunc env => MonadRipper (RIO env) where
  rip = ripOneStream
  delayReconnect = delayWithLog
  shouldRepeat = pure True

-- | The endless ripping loop.
ripper :: (MonadRipper m) => Request -> Maybe FilePath -> Int -> Int -> m ()
ripper request maybeOutputDir reconnectDelay smallReconnectDelay = go mempty
  {-
   - I tried to use `StateT Any m` like a good citizen at first, but couldn't
   - make it work:
   -
   - * `repeatForever` can't be used because its parameter is in monad `m`,
   - which is the same as the output monad, and the inside monad can't be the
   - same as the outside monad: `m != StateT Any m` ?!
   -
   - * explicit infinite recursion is hard to stop in tests; throwing an
   - exception causes more issues (exception handling prevents the receiving of
   - the result value in `runTestM`)
   -}
  where
    go wasEverSuccessfulRip = do
      result <- rip request maybeOutputDir

      let isSuccessfulRip = result == RipRecorded
          wasEverSuccessfulRip' = wasEverSuccessfulRip <> Any isSuccessfulRip

      delayReconnect $ if getAny wasEverSuccessfulRip' then smallReconnectDelay else reconnectDelay
      whenM shouldRepeat $ go wasEverSuccessfulRip'

delayWithLog :: (MonadIO m, MonadReader env m, HasLogFunc env) => Int -> m ()
delayWithLog reconnectDelay = do
  logDebug "Disconnected"
  threadDelay reconnectDelay
  logDebug "Reconnecting"

-- | Rips a stream for as long as the connection is open.
ripOneStream :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => Request -> Maybe FilePath -> m RipResult
ripOneStream request maybeOutputDir =
  runResourceT . handle httpExceptionHandler . httpSink request $ \response -> do
    logInfo . displayShow . getResponseStatus $ response

    {-
    - I want to include the time at which we start receiving the response body
    - (ideally, the body's first byte, not the header) in the filename;
    - in this block, we have the `response` and are ready to stream the body,
    - so this time should be good enough
    -}

    filename <- liftIO getFilename
    sinkFile $ maybe filename (</> filename) maybeOutputDir
    pure RipRecorded

httpExceptionHandler :: (MonadIO m, MonadReader env m, HasLogFunc env) => HttpException -> m RipResult
httpExceptionHandler e = do
  logError $ case e of
    -- for http exceptions, we don't print the request, only the exception details
    HttpExceptionRequest _ content -> case content of
      StatusCodeException response _ -> "Unsuccessful response: " <> displayShow (getResponseStatus response)
      _ -> displayShow content
    -- for invalid url exceptions, we print it as is
    _ -> displayShow e

  -- FIXME this is not necessarily true because, based on logs, the recording
  -- started, but after 16 minutes the library threw an `IncompleteHeaders`
  -- exception
  pure RipNothing

-- | Converts the number of seconds to the number of microseconds expected by `timeout`.
secondsToTimeout :: Float -> Int
secondsToTimeout = round . (* seconds)
  where seconds = 1_000_000

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
