{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ripper.Run
  ( MonadRipper(..)
  , RipResult(..)
  , handleResourceVanished
  , run, ripper
  ) where

import Conduit
import Data.Monoid (Last(..))
import Data.Time.TZTime
import Network.HTTP.Conduit (HttpExceptionContent(..))
import Network.HTTP.Simple
import RIO.Directory (createDirectoryIfMissing)
import RIO.FilePath ((</>))
import RIO.State
import qualified RIO.Text as T
import RIO.Time
import System.IO.Error (isResourceVanishedError)

import Ripper.Import
import Ripper.RipperDelay
import RSSGen.Duration

run :: RIO App ()
run = do
  ripIntervals <- getRipIntervals

  options <- asks appOptions
  let maybeOutputDir = optionsOutputDirectory options
  for_ maybeOutputDir ensureDirectory

  let ripTimeout = toMicroseconds $ optionsRipLength options

  userAgent <- asks appUserAgent

  void . timeout ripTimeout
    $ ripper userAgent maybeOutputDir ripIntervals (optionsStreamConfig options)

-- | Returns parsed `RipperInterval`s from the `Options`. Terminates the program
-- with an error message if an interval can't be parsed.
getRipIntervals :: (MonadIO m, MonadReader env m, HasAppOptions env) => m [RipperInterval]
getRipIntervals = do
  options <- view appOptionsL
  let ripIntervalRefs = optionsRipIntervalRefs options
  eitherIntervals <- liftIO $ traverse ripperIntervalFromRef ripIntervalRefs
  let (errors, intervals) = partitionEithers eitherIntervals
  if not (null errors)
    then error $ "Couldn't parse rip intervals: " <> show errors
    else pure intervals

-- | The result of a single rip call. The information conveyed by this type
-- is whether the call has received and saved any data.
data RipResult = RipRecorded RipEndTime | RipNothing
  deriving (Eq, Show)

ripEndTimeFromResult :: RipResult -> Maybe RipEndTime
ripEndTimeFromResult (RipRecorded t) = Just t
ripEndTimeFromResult RipNothing = Nothing

class Monad m => MonadRipper m where
  rip :: Request -> Maybe FilePath -> m RipResult
  getRipDelay :: [RipperInterval] -> Maybe RipEndTime -> Now -> m RetryDelay
  -- TODO can `MonadTime` be composed here to replace these two functions?
  getTime :: m Now
  delayReconnect :: RetryDelay -> m ()
  shouldRepeat :: m Bool

instance HasLogFunc env => MonadRipper (RIO env) where
  rip = ripOneStream
  getRipDelay is ripEnd now = pure $ getRipperDelay is ripEnd now
  getTime = liftIO getCurrentTZTime
  delayReconnect = delayWithLog
  shouldRepeat = pure True

-- | The endless ripping loop.
ripper :: (MonadRipper m) => Text -> Maybe FilePath -> [RipperInterval] -> StreamConfig -> m ()
ripper userAgent maybeOutputDir ripperIntervals streamConfig = evalStateT go mempty
  {-
   - * `repeatForever` can't be used because its parameter is in monad `m`,
   - which is the same as the output monad, and the inside monad can't be the
   - same as the outside monad: `m != StateT Any m` ?!
   -
   - * explicit infinite recursion is hard to stop in tests; throwing an
   - exception causes more issues (exception handling prevents the receiving of
   - the result value in `runTestM`)
   -}
  where
    go :: MonadRipper m => StateT (Last RipEndTime) m ()
    go = do
      let (StreamURL url) = case streamConfig of
            -- FIXME move the live stream check for this case
            StreamConfig _name url' -> url'
            SimpleURL url' -> url'
      let request = mkRipperRequest userAgent url
      result <- lift $ rip request maybeOutputDir

      modify' (<> Last (ripEndTimeFromResult result))
      maybeLatestRipEndTime <- gets getLast

      -- TODO how to get rid of `lift`s?
      lift $ do
        now <- getTime
        delay <- getRipDelay ripperIntervals maybeLatestRipEndTime now
        delayReconnect delay
      whenM (lift shouldRepeat) go

delayWithLog :: (MonadIO m, MonadReader env m, HasLogFunc env) => RetryDelay -> m ()
delayWithLog reconnectDelay = do
  logDebug "Disconnected"
  threadDelay . toMicroseconds . toDuration $ reconnectDelay
  logDebug "Reconnecting"

-- | Rips a stream for as long as the connection is open.
ripOneStream :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => Request -> Maybe FilePath -> m RipResult
ripOneStream request maybeOutputDir = do
  {-
   - I need to know if `httpSink` receives a successful response and records
   - anything even in the case an exception is then thrown; I couldn't find a
   - better (purer) way to extract a flag from within the closure than an `IORef`,
   - using a `WriterT` complained about:
   - `• Could not deduce (MonadUnliftIO (WriterT Any m)) arising from a use of ‘runResourceT’
        from the context: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)`
   - As the doc says, `MonadUnliftIO` doesn't work with stateful monads :(
   -}
  recordedAnythingVar <- newIORef False

  ripResult <- runResourceT
    . handleResourceVanished
    . handle httpExceptionHandler
    . httpSink request $ \response -> do
      logInfo . displayShow . getResponseStatus $ response

      writeIORef recordedAnythingVar True

      {-
      - I want to include the time at which we start receiving the response body
      - (ideally, the body's first byte, not the header) in the filename;
      - in this block, we have the `response` and are ready to stream the body,
      - so this time should be good enough
      -}

      filename <- liftIO getFilename
      sinkFile $ maybe filename (</> filename) maybeOutputDir
      -- we're not inside `MonadRipper` here, so we're using the original IO func
      now <- liftIO getCurrentTZTime
      pure $ RipRecorded now

  {-
   - after a possible http exception is handled, we need to figure out if
   - anything was recorded (i.e. we got a successful response): because an
   - exception can be thrown in the middle of the connection, we also need to
   - consider the value of `recordedAnythingVar`
   -}
  case ripResult of
    r@(RipRecorded _) -> pure r
    RipNothing -> do
      recordedAnything <- readIORef recordedAnythingVar
      now <- liftIO getCurrentTZTime
      pure $ if recordedAnything then RipRecorded now else RipNothing

httpExceptionHandler :: (MonadIO m, MonadReader env m, HasLogFunc env) => HttpException -> m RipResult
httpExceptionHandler e = do
  logError $ case e of
    -- for http exceptions, we don't print the request, only the exception details
    HttpExceptionRequest _ content -> case content of
      StatusCodeException response _ -> "Unsuccessful response: " <> displayShow (getResponseStatus response)
      _ -> displayShow content
    -- for invalid url exceptions, we print it as is
    _ -> displayShow e

  -- this is not necessarily true because, based on logs, the recording
  -- started, but after 16 minutes the library threw an `IncompleteHeaders`
  -- exception, so there is an extra step to verify it *after* the exception
  -- handler; see `ripOneStream`
  pure RipNothing

-- | Handles the `ResourceVanished` IOError in order not to crash the program.
-- It's now handled because "Network.Socket.recvBuf: resource vanished (Connection reset by peer)"
-- crashed the ripper once while recording RCMP, apparently caused by a
-- TCP RST packet.
handleResourceVanished :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => m RipResult -> m RipResult
handleResourceVanished = handleJust
  (\e -> if isResourceVanishedError e then Just e else Nothing)
  -- note: it may look strange that the same exception `e` is passed to the handler
  -- after filtering, so I could use `handle` to avoid that, but that implementation
  -- would require manual rethrowing of all other types of exceptions
  (\e -> logError (displayShow e) >> pure RipNothing)

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

mkRipperRequest :: Text -> URL -> Request
-- note: this causes impure exceptions for invalid URLs
mkRipperRequest userAgent = setUserAgent userAgent . parseRequestThrow_ . T.unpack . urlToText

setUserAgent :: Text -> Request -> Request
setUserAgent = addRequestHeader "User-Agent" . encodeUtf8
