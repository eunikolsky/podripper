module Main_Run
  ( RipName
  , runParser
  ) where

import Data.Text (Text)
import Options.Applicative.Simple

type RipName = Text

runParser :: Parser RipName
runParser = strArgument
  (metavar "RIP_NAME" <> help "Rip config name")
