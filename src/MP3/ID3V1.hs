module MP3.ID3V1
  ( id3Parser
  ) where

import Control.Monad
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as A

-- | Parses the ID3 v1 tag. The tag must be at the end of an MP3 file, if
-- present, but the EOF is not checked here (for better composability).
id3Parser :: Parser ()
id3Parser = void $ A.string "TAG" >> A.take (tagSize - 3)

-- | The ID3 v1 tag is always 128 bytes long.
tagSize :: Int
tagSize = 128
