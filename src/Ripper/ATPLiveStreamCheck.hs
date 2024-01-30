module Ripper.ATPLiveStreamCheck
  ( checkATPLiveStream
  , extractURL
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Aeson hiding ((<?>))
import Data.Aeson.KeyMap qualified as A
import Data.Functor
import Data.List (find)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Text.Lazy.IO qualified as TL
import Network.HTTP.Simple
import RipConfig
import Ripper.Types
import Text.XML.Light

-- | Checks whether the ATP's stream is live and if so, extracts the stream URL
-- from the status response.
--
-- The ATP support is hardcoded in the program because its live stream check
-- is more complicated and the stream URL needs to be extracted from the
-- status endpoint.
-- FIXME support this via the config file
checkATPLiveStream :: StreamCheckConfig -> IO (Maybe StreamURL)
checkATPLiveStream StreamCheckConfig{checkURL, liveKey, playerKey} = handleError <=< runExceptT $ do
  statusResponse <- liftIO . fmap getResponseBody . httpLBS . parseRequest_
    . T.unpack . urlToText . getStreamCheckURL $ checkURL
  liftIO . TL.putStrLn $ TLE.decodeUtf8 statusResponse
  status <- liftEither . eitherDecode @Object $ statusResponse

  isLiveValue <- liftEither $ A.lookup liveKey status
    <?> ("Can't find `" <> show liveKey <> "` key")
  isLive <- liftEither $ extractBool isLiveValue

  pure $ if isLive then retrieveStreamURL playerKey status else Nothing

extractBool :: Value -> Either String Bool
extractBool (Bool b) = Right b
extractBool x = Left $ "Expected a bool, got " <> show x

asText :: Value -> Maybe Text
asText (String t) = Just t
asText _ = Nothing

retrieveStreamURL :: A.Key -> Object -> Maybe StreamURL
retrieveStreamURL playerKey status = A.lookup playerKey status >>= asText >>= extractURL

extractURL :: Text -> Maybe StreamURL
extractURL t = do
  xmlElem <- listToMaybe . onlyElems $ parseXML t
  src <- T.pack <$> firstJust [findAudioSourceSrc xmlElem, findAudioSrc xmlElem]
    <|> findFirstURL t
  pure . StreamURL . URL $ src

  where
    findAudioSourceSrc = srcAttr <=< findElement (unqual "source") <=< audioElem
    findAudioSrc = srcAttr <=< audioElem
    findFirstURL = find ("http" `T.isPrefixOf`) . T.split (== '"')

    audioElem = findElement (unqual "audio")
    srcAttr = findAttr (unqual "src")

firstJust :: Foldable t => t (Maybe a) -> Maybe a
firstJust = getFirst . foldMap First

handleError :: Either String (Maybe a) -> IO (Maybe a)
handleError (Right b) = pure b
handleError (Left err) = putStrLn err $> Nothing

-- infix 7 <?>
(<?>) :: Maybe a -> b -> Either b a
Just x <?> _ = Right x
Nothing <?> e = Left e
