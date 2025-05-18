{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Ripper.Run
  ( MonadRipper(..)
  , RipResult(..)
  , handleResourceVanished
  , run, ripper
  ) where

import Conduit
import Control.Concurrent.STM.TBMQueue
import Data.Conduit.Attoparsec
import Data.Conduit.List qualified as C
import Data.Maybe
import Data.Monoid (Last(..))
import Data.Time.TZTime
import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed.Mutable (IOVector)
import Data.Vector.Unboxed.Mutable qualified as UMV
import Network.HTTP.Conduit (HttpExceptionContent(..))
import Network.HTTP.Simple
import RIO.Directory (createDirectoryIfMissing)
import RIO.FilePath
import RIO.State
import qualified RIO.Text as T
import RIO.Time
import System.IO.Error (isResourceVanishedError)

import MP3.Parser
import MP3.Xing
import Ripper.LiveStreamCheck
import Ripper.Import
import Ripper.RipperDelay
import RSSGen.Duration

run :: RIO App ()
run = do
  ripIntervals <- getRipIntervals

  options <- asks appOptions
  let maybeRawRipsDir = optionsRawRipsDirectory options
      maybeCleanRipsDir = optionsCleanRipsDirectory options
  mapM_ ensureDirectory $ catMaybes [maybeRawRipsDir, maybeCleanRipsDir]

  userAgent <- asks appUserAgent

  maybeTimeout (optionsRipLength options)
    $ ripper userAgent maybeRawRipsDir maybeCleanRipsDir ripIntervals (optionsStreamConfig options)

maybeTimeout :: MonadUnliftIO m => Maybe Duration -> m () -> m ()
maybeTimeout (Just d) = void . timeout (toMicroseconds d)
maybeTimeout Nothing = id

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
data RipResult = RipRecorded !SuccessfulRip !RipEndTime | RipNothing
  deriving (Eq, Show)

withRecordedRip :: ((SuccessfulRip, RipEndTime) -> a) -> RipResult -> Maybe a
withRecordedRip f (RipRecorded r time) = Just $ f (r, time)
withRecordedRip _ RipNothing = Nothing

class Monad m => MonadRipper m where
  rip :: (StreamURL, Request) -> Maybe FilePath -> Maybe FilePath -> m RipResult
  checkLiveStream :: StreamURLConfig -> m (Maybe StreamURL)
  getRipDelay :: [RipperInterval] -> Maybe RipEndTime -> Now -> m RetryDelay
  -- TODO can `MonadTime` be composed here to replace these two functions?
  getTime :: m Now
  delayReconnect :: RetryDelay -> m ()
  shouldRepeat :: m Bool
  notifyRip :: SuccessfulRip -> m ()

instance (HasLogFunc env, HasAppRipsQueue env, HasAppOptions env) => MonadRipper (RIO env) where
  rip = ripOneStream
  checkLiveStream (StreamWithLiveCheck streamCheckConfig) = liftIO . getLiveStreamURL $ streamCheckConfig
  checkLiveStream (StreamWithURL url) = pure $ Just url
  getRipDelay is ripEnd now = do
    options <- view appOptionsL
    let defaultDelay = optionsDefaultRipDelay options
        postRipEndDelays = optionsPostRipEndDelays options
    pure $ getRipperDelay (defaultDelay, postRipEndDelays) is ripEnd now
  getTime = liftIO getCurrentTZTime
  delayReconnect = delayWithLog
  shouldRepeat = pure True
  notifyRip r = do
    ripsQueue <- view appRipsQueueL
    atomically $ writeTQueue ripsQueue r

-- | The endless ripping loop.
ripper :: (MonadRipper m) => Text -> Maybe FilePath -> Maybe FilePath -> [RipperInterval] -> StreamConfig -> m ()
ripper userAgent maybeRawRipsDir maybeCleanRipsDir ripperIntervals streamConfig = evalStateT go mempty
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
      maybeStreamURL <- lift $ urlFromStreamConfig streamConfig
      case maybeStreamURL of
        Just streamURL@(StreamURL url) -> do
          let request = mkRipperRequest userAgent url
          result <- lift $ rip (streamURL, request) maybeRawRipsDir maybeCleanRipsDir
          modify' (<> Last (withRecordedRip snd result))

          maybe (pure ()) (lift . notifyRip) $ withRecordedRip fst result

        Nothing -> pure ()

      maybeLatestRipEndTime <- gets getLast

      -- TODO how to get rid of `lift`s?
      lift $ do
        now <- getTime
        delay <- getRipDelay ripperIntervals maybeLatestRipEndTime now
        delayReconnect delay
      whenM (lift shouldRepeat) go

urlFromStreamConfig :: MonadRipper m => StreamConfig -> m (Maybe StreamURL)
urlFromStreamConfig (StreamConfig urlConfig) = checkLiveStream urlConfig
urlFromStreamConfig (SimpleURL url) = pure . Just $ url

delayWithLog :: (MonadIO m, MonadReader env m, HasLogFunc env) => RetryDelay -> m ()
delayWithLog reconnectDelay = do
  logDebug $ "Disconnected; waiting for " <> displayShow reconnectDelay
  threadDelay . toMicroseconds . toDuration $ reconnectDelay
  logDebug "Reconnecting"

-- | Information about a rip in progress; the difference from `SuccessfulRip` is
-- the mutable vector of frames, which should provide a better memory usage than
-- appending to an immutable vector all the time.
data InProgressRip = InProgressRip
  { irFilename :: !FilePath
  , irFrames :: !(IOVector FrameInfo)
  -- ^ mutable vector for storing frames
  , irWriteIndex :: !Int
  -- ^ next index, into which the frame should be added; this is not the same as
  -- vector's `length` because the vector may contain uninitialized elements
  }

toSuccessfulRip :: MonadIO m => StreamURL -> InProgressRip -> m SuccessfulRip
toSuccessfulRip streamURL rip' = do
  let writtenLength = irWriteIndex rip'
  -- this is safe because ripping is done at this point
  -- it's not clear whether `V.force` is worth it here
  frames <- liftIO . fmap UV.force . UV.unsafeFreeze . UMV.take writtenLength $ irFrames rip'
  pure $ SuccessfulRip
    { ripFilename = irFilename rip'
    , ripMP3Structure = MP3Structure frames
    , ripStreamURL = Just streamURL
    }

-- | How many new elements to add to the frames vector when it's fully filled.
vectorSizeInc :: Int
vectorSizeInc = 10 * framesInMinute
  -- | 44100 Hz MPEG1 Layer3 frames
  where framesInMinute = 2297

-- | Rips a stream for as long as the connection is open.
ripOneStream
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasAppOptions env)
  => (StreamURL, Request) -> Maybe FilePath ->  Maybe FilePath -> m RipResult
ripOneStream (streamURL, request) maybeRawRipsDir maybeCleanRipsDir = do
  noDataTimeout <- optionsNoDataTimeout <$> view appOptionsL

  {-
   - I need to know if `httpSink` receives a successful response and records
   - anything even in the case an exception is then thrown; I couldn't find a
   - better (purer) way to extract a flag from within the closure than an `IORef`,
   - using a `WriterT` complained about:
   - `• Could not deduce (MonadUnliftIO (WriterT Any m)) arising from a use of ‘runResourceT’
        from the context: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)`
   - As the doc says, `MonadUnliftIO` doesn't work with stateful monads :(
   -}
  maybeRipVar <- newIORef Nothing

  ripResult <- runResourceT
    . handleStalledException
    . handleResourceVanished
    . handle httpExceptionHandler
    -- `httpSink`'s and `httpSource`'s` types don't match with `doesn'tStall`'s type
    . withResponse request $ \response -> do
      logInfo . displayShow . getResponseStatus $ response

      {-
      - I want to include the time at which we start receiving the response body
      - (ideally, the body's first byte, not the header) in the filename;
      - in this block, we have the `response` and are ready to stream the body,
      - so this time should be good enough
      -}
      basename <- liftIO getBasename
      let filename = maybe basename (</> basename) maybeCleanRipsDir <.> "mp3"
          rawBasename = basename <> "_raw" <.> "mp3"
          rawFilename = maybe rawBasename (</> rawBasename) maybeRawRipsDir
      framesVector <- liftIO $ UMV.new vectorSizeInc
      writeIORef maybeRipVar . Just $ InProgressRip
        { irFilename = filename
        , irFrames = framesVector
        , irWriteIndex = 0
        }

      let dumpBody body =
            let rawDump = sinkFile rawFilename

                saveCleanDump = mapC (getFrameData . fData) .| sinkFile filename
                processCleanDump = mapM_C $ extendRip maybeRipVar . fInfo
                cleanDump = conduitParserEither maybeFrameParser
                  .| C.mapMaybeM getMP3Frame
                  .| getZipSink (ZipSink saveCleanDump *> ZipSink processCleanDump)

                bothDumps = getZipSink $ ZipSink rawDump *> ZipSink cleanDump
            in runConduit $ body .| bothDumps
      -- there was a strange issue with ATP when the recording started, but then
      -- halted about an hour later (the file modtime confirms this) in the
      -- middle of the stream, whereas the TCP connection itself was left open
      -- for many hours, even when a new connection returned `404`! this conduit
      -- wrapper ensures that if there is no data for the given duration, we'll
      -- disconnect and reconnect
      if isEmpty noDataTimeout
        -- empty timeout => don't detect stalled connections
        then dumpBody $ getResponseBody response
        else doesn'tStall noDataTimeout (getResponseBody response) dumpBody

      -- we're not inside `MonadRipper` here, so we're using the original IO func
      now <- liftIO getCurrentTZTime
      -- `maybeRipVar` can't be `Nothing` in this block
      inProgressRip <- fromJust <$> readIORef maybeRipVar
      recordedRip <- toSuccessfulRip streamURL inProgressRip
      pure $ RipRecorded recordedRip now

  {-
   - after a possible http exception is handled, we need to figure out if
   - anything was recorded (i.e. we got a successful response): because an
   - exception can be thrown in the middle of the connection, we also need to
   - consider the value of `maybeFilenameVar`
   -}
  case ripResult of
    r@(RipRecorded _ _) -> pure r
    RipNothing -> do
      maybeRip <- readIORef maybeRipVar
      now <- liftIO getCurrentTZTime
      case maybeRip of
        Just inProgressRip -> do
          recordedRip <- toSuccessfulRip streamURL inProgressRip
          pure $ RipRecorded recordedRip now
        Nothing -> pure RipNothing

extendRip :: MonadIO m => IORef (Maybe InProgressRip) -> FrameInfo -> m ()
extendRip maybeRipVar frame = modifyIORefIO' maybeRipVar $ \case
  Just rip' -> do
    let framesVector = irFrames rip'
        writeIndex = irWriteIndex rip'

    let isVectorFull = writeIndex >= UMV.length framesVector
    framesVector' <- if isVectorFull
      then liftIO $ UMV.grow framesVector vectorSizeInc
      else pure framesVector
    liftIO $ UMV.write framesVector' writeIndex frame

    pure . Just $ rip'
      { irFrames = framesVector'
      , irWriteIndex = writeIndex + 1
      }
  Nothing -> pure Nothing

modifyIORefIO' :: MonadIO m => IORef a -> (a -> m a) -> m ()
modifyIORefIO' ref f = do
  x <- readIORef ref
  x' <- f x
  x' `seq` writeIORef ref x'

getMP3Frame :: (MonadReader env m, HasLogFunc env, MonadIO m)
  => Either ParseError (PositionRange, MaybeFrame) -> m (Maybe Frame)
getMP3Frame (Right (_, Valid f)) = pure $ Just f
getMP3Frame (Right (posRange, Junk l)) = do
  logInfo $ mconcat
    [ "Found junk in stream: "
    , displayShow l, " bytes long at bytes "
    , displayShow . posOffset . posRangeStart $ posRange
    , "-"
    , displayShow . posOffset . posRangeEnd $ posRange
    ]
  pure Nothing
getMP3Frame (Left e) = do
  logError $ "Parse error: " <> displayShow e
  pure Nothing

-- TODO for some reason, this handler doesn't catch an `InternalException` about
-- an unknown CA (when using `mitmproxy`):
-- ```
-- operation failed: HttpExceptionRequest Request {
--   …
-- }
--  (InternalException (HandshakeFailed (Error_Protocol ("certificate has unknown CA",True,UnknownCa))))
-- ```
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

handleStalledException :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => m RipResult -> m RipResult
handleStalledException = handle $ \(e :: StalledException) ->
  logError (displayShow e) >> pure RipNothing

ensureDirectory :: MonadIO m => FilePath -> m ()
ensureDirectory = liftIO . createDirectoryIfMissing createParents
  where createParents = True

-- | Returns a rip base filename (w/o extension) that follows the
-- `streamripper`'s pattern: `sr_program_YYYY_mm_dd_HH_MM_SS` in the current
-- timezone.
--
-- https://en.wikipedia.org/wiki/Streamripper
getBasename :: IO FilePath
getBasename = do
  time <- getCurrentLocalTime
  let timeString = formatTime defaultTimeLocale "%_Y_%m_%d_%H_%M_%S" time
  pure $ "sr_program_" <> timeString

  where
    getCurrentLocalTime :: MonadIO m => m LocalTime
    getCurrentLocalTime = zonedTimeToLocalTime <$> getZonedTime

mkRipperRequest :: Text -> URL -> Request
-- note: this causes impure exceptions for invalid URLs
mkRipperRequest userAgent = setUserAgent userAgent . parseRequestThrow_ . T.unpack . urlToText

setUserAgent :: Text -> Request -> Request
setUserAgent = addRequestHeader "User-Agent" . encodeUtf8

data StalledException = StalledException
  deriving Show

instance Exception StalledException

-- | Ensures that a conduit produces output periodically, not slower than once
-- every time `duration`. If there is no output for longer than that,
-- `StalledException` is thrown.
--
-- "The basic idea is to have two sibling threads: one running the original
-- source and writing its values to a queue, and another running the full
-- conduit pipeline with a modified source that will time out on reads from
-- that queue."
-- — https://mail.haskell.org/pipermail/haskell-cafe/2018-June/129314.html
--
-- Source: https://gist.github.com/snoyberg/7e5dd52109b03c8bf1aa8fe1a7e522b9
doesn'tStall
  :: MonadUnliftIO m
  => Duration
  -> ConduitT () o m () -- ^ original source
  -> (ConduitT () o m () -> m a) -- ^ what to do with modified source
  -> m a
doesn'tStall duration src inner = do
  queue <- liftIO $ newTBMQueueIO 2
  runConcurrently $
    Concurrently (filler queue) *>
    Concurrently (inner $ consumer queue)
  where
    filler queue =
      runConduit (src .| mapM_C (atomically . writeTBMQueue queue))
      `finally` atomically (closeTBMQueue queue)

    consumer queue =
        loop
      where
        loop = do
          res <- lift $ timeout (toMicroseconds duration) $ atomically $ readTBMQueue queue
          case res of
            -- timeout occurred
            Nothing -> throwIO StalledException

            -- queue is closed
            Just Nothing -> pure ()

            -- more data available
            Just (Just o) -> yield o >> loop
