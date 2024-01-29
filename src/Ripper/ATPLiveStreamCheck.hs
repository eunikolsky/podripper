module Ripper.ATPLiveStreamCheck
  ( checkATPLiveStream
  , extractURL
  ) where

import Control.Monad
import Control.Monad.Except
import Data.Aeson hiding ((<?>))
import Data.Aeson.KeyMap qualified as A
import Data.Char
import Data.Functor
import Data.List (dropWhileEnd)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Text.Lazy.IO qualified as TL
import Network.HTTP.Simple
import RipConfig
import Ripper.Types
import Ripper.Util

-- | Checks whether the ATP's stream is live and if so, extracts the stream URL
-- from the status response. If the stream is live, but the stream URL can't be
-- found, uses the `originalStreamURL` (from the config).
--
-- The ATP support is hardcoded in the program because its live stream check
-- is more complicated and the stream URL needs to be extracted from the
-- status endpoint.
-- FIXME support this via the config file
checkATPLiveStream :: StreamURL -> IO (Maybe StreamURL)
checkATPLiveStream originalStreamURL = handleError <=< runExceptT $ do
  statusResponse <- liftIO . fmap getResponseBody . httpLBS $ parseRequest_ "https://atp.fm/livestream_status"
  liftIO . TL.putStrLn $ TLE.decodeUtf8 statusResponse
  status <- liftEither . eitherDecode @Object $ statusResponse

  isLiveValue <- liftEither $ A.lookup "live" status <?> "Can't find `live` key"
  isLive <- liftEither $ extractBool isLiveValue

  if isLive then liftIO (Just <$> retrieveStreamURL originalStreamURL status) else pure Nothing

extractBool :: Value -> Either String Bool
extractBool (Bool b) = Right b
extractBool x = Left $ "Expected a bool, got " <> show x

asString :: Value -> Maybe String
asString (String t) = Just $ T.unpack t
asString _ = Nothing

retrieveStreamURL :: StreamURL -> Object -> IO StreamURL
retrieveStreamURL originalStreamURL status = fromMaybe originalStreamURL <$>
  case A.lookup "player" status >>= asString of
    Just player -> do
      -- FIXME replace with a native Haskell solution
      maybeAudioSourceSrc <- readCommandNonEmpty "htmlq" ["-a", "src", "audio source"] player
      maybeAudioSrc <- readCommandNonEmpty "htmlq" ["-a", "src", "audio"] player
      maybeFirstLink <- readCommandNonEmpty "sed" ["-nE", "s/.*\"(http[^\"]+)\".*/\\1/p"] player
      -- if I understand correctly, all three values are not lazy and are
      -- evaluated regardless of whether the previous one was a `Just`; if so,
      -- it's not a big deal as this function isn't called often
      let firstMaybe = getFirst $ foldMap First [maybeAudioSourceSrc, maybeAudioSrc, maybeFirstLink]
      pure $ StreamURL . URL . T.pack <$> firstMaybe

    Nothing -> pure Nothing

extractURL :: Text -> Maybe StreamURL
extractURL = const . Just . StreamURL . URL $ "https://example.org/"

-- | Wrapper around `readCommand` that returns `Nothing` if the output is
-- whitespace only.
readCommandNonEmpty :: String -> [String] -> String -> IO (Maybe String)
readCommandNonEmpty prog args input = readCommand prog args input <&> (>>= skipEmpty)

skipEmpty :: String -> Maybe String
skipEmpty s = let trimmed = trimSpace s
  in if null trimmed then Nothing else Just trimmed

trimSpace :: String -> String
trimSpace = dropWhile isSpace . dropWhileEnd isSpace

handleError :: Either String (Maybe a) -> IO (Maybe a)
handleError (Right b) = pure b
handleError (Left err) = putStrLn err $> Nothing

-- infix 7 <?>
(<?>) :: Maybe a -> b -> Either b a
Just x <?> _ = Right x
Nothing <?> e = Left e
