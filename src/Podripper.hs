module Podripper
  ( RipName
  , main
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (die)

type RipName = Text

main :: RipName -> IO ()
main ripName = die $ mconcat ["TODO implement ripping ", T.unpack ripName, "!"]
