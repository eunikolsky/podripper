module Main_Ripper
  ( main
  , ripperParser
  ) where

import Data.Version (showVersion)
import Import
import Options.Applicative.Simple
import qualified Paths_ripper
import RIO.Process
import qualified RIO.Text as T
import Run

main :: Options -> IO ()
main options = do
  lo <- setLogUseLoc False <$> logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          , appUserAgent = "ripper/" <> T.pack (showVersion Paths_ripper.version)
          }
     in runRIO app run

ripperParser :: Parser Options
ripperParser = Options
  <$> switch ( long "verbose"
            <> short 'v'
            <> help "Verbose output?"
            )
  <*> optional (
      strOption ( short 'd'
                <> help "Directory to put rips into"
                <> metavar "DIR"
                )
      )
  <*> option auto ( short 'l'
                <> help "How long to rip, in seconds"
                <> metavar "seconds"
                  )
  <*> option auto ( short 'r'
                <> help "Reconnect delay, in seconds"
                <> metavar "reconnect_delay"
                <> value 5
                <> showDefault
                  )
  <*> option auto ( short 's'
                <> help "Small reconnect delay, in seconds"
                <> metavar "reconnect_delay"
                <> value 1
                <> showDefault
                  )
  <*> strArgument ( metavar "URL"
                <> help "Stream URL"
                  )
