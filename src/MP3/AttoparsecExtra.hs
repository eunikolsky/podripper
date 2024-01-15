module MP3.AttoparsecExtra
  ( getPos
  , takeUpTo
  ) where

import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.Internal.Types qualified as AI (Parser(..), fromPos)
import Data.ByteString (ByteString)

-- | Consumes up to `n` bytes of input. Returns everything consumed so far if
-- EOF is encountered; if nothing is consumed before EOF, returns an empty
-- string. An "extended" version of `attoparsec`'s `take` combinator.
takeUpTo :: Int -> Parser ByteString
takeUpTo n = A.scan 0 (\len _ -> if len >= n then Nothing else Just (len + 1))

-- | Retrieves the current position.
--
-- Note: this uses private API to get this data, but it should be fine for now.
-- Source: https://github.com/haskell/attoparsec/issues/101
getPos :: AI.Parser i Int
getPos = AI.Parser $ \t pos more _ succ' -> succ' t pos more (AI.fromPos pos)
