module MP3.ID3
  ( ID3Fields(..)
  , ID3Header(..)
  , generateID3Header
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Builder qualified as BSB
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import Data.Word (Word32)
import MP3.AudioDuration

data ID3Fields = ID3Fields
  { id3Title :: !Text
  , id3Artist :: !Text
  , id3Album :: !Text
  , id3RecordingTime :: !UTCTime
  , id3Genre :: !Text
  , id3Publisher :: !Text
  , id3Duration :: !AudioDuration
  , id3EncodingTime :: !UTCTime
  , id3Language :: !Text
  -- ^ A three-letter ISO-639-2 language code
  }

newtype ID3Header = ID3Header { getID3Header :: ByteString }

-- | Generates an ID3 v2.4 header with the given fields.
generateID3Header :: ID3Fields -> ID3Header
generateID3Header ID3Fields
    { id3Title, id3Artist, id3Album, id3RecordingTime, id3Genre, id3Publisher
    , id3Duration, id3EncodingTime, id3Language
    } =
  ID3Header . BS.toStrict . BSB.toLazyByteString $ header <> BSB.lazyByteString frames

  where
    header = mconcat [id3Id, version, flags, size]
    id3Id = "ID3"
    version = "\x04\x00"
    flags = BSB.word8 0
    size = BSB.word32BE . syncSafe . fromIntegral . BSL.length $ frames

    frames = BSB.toLazyByteString $ mconcat
      [ textFrame frameTitle id3Title
      , textFrame frameLeadPerformer id3Artist
      , textFrame frameAlbum id3Album
      , textFrame frameRecordingTime $ formatID3Time id3RecordingTime
      , textFrame frameContentType id3Genre
      , textFrame framePublisher id3Publisher
      , textFrame frameLength $ inMilliseconds id3Duration
      , textFrame frameEncodingTime $ formatID3Time id3EncodingTime
      , textFrame frameLanguage id3Language
      ]

frameTitle, frameLeadPerformer, frameAlbum, frameRecordingTime
  , frameContentType, framePublisher, frameLength, frameEncodingTime
  , frameLanguage :: ByteString
frameTitle = "TIT2"
frameLeadPerformer = "TPE1"
frameAlbum = "TALB"
frameRecordingTime = "TDRC"
frameContentType = "TCON"
framePublisher = "TPUB"
frameLength = "TLEN"
frameEncodingTime = "TDEN"
frameLanguage = "TLAN"

formatID3Time :: UTCTime -> Text
formatID3Time = T.pack . formatTime defaultTimeLocale "%FT%T"

inMilliseconds :: AudioDuration -> Text
inMilliseconds = T.pack . show @Int . floor . (* 1000) . getAudioDuration

textFrame
  :: ByteString
  -- ^ Frame ID, must be 4 characters
  -> Text
  -- ^ Frame value
  -> BSB.Builder
textFrame frameId value =
  mconcat [BSB.byteString frameId, size, flags, BSB.byteString contents]

  where
    size = BSB.word32BE . syncSafe . fromIntegral . BS.length $ contents
    flags = BSB.word16BE 0

    contents = mconcat [utf8Marker, utf8Value, utf8Terminator]
    utf8Marker = "\x03"
    utf8Value = TE.encodeUtf8 value
    utf8Terminator = "\x00"

-- | Returns a "synchsafe integer" for the given integer: the most-significant
-- bit of every byte is reset, thus it can store 28 bits of information.
syncSafe :: Word32 -> Word32
syncSafe x =
  (x .&. 0x7f)
  .|. ((x `shiftL` 1) .&. 0x7f00)
  .|. ((x `shiftL` 2) .&. 0x7f0000)
  .|. ((x `shiftL` 3) .&. 0x7f000000)
