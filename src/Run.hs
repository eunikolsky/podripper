module Run
  ( RipName
  , runParser
  ) where

import Options.Applicative
import Podripper (RipName)

runParser :: Parser RipName
runParser = strArgument
  (metavar "RIP_NAME" <> help "Rip config name")
