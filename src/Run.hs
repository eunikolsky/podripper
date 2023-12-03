module Run
  ( RipName
  , runParser
  ) where

import Options.Applicative
import Ripper.Types (RipName)

runParser :: Parser RipName
runParser = strArgument
  (metavar "RIP_NAME" <> help "Rip config name")
