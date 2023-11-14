module Ripper.Main
  ( ripperParser
  , run
  ) where

import Data.Version (showVersion)
import RSSGen.Duration
import Ripper.Import
import Options.Applicative
import qualified Paths_ripper
import RIO.Process
import qualified RIO.Text as T
import qualified Ripper.Run

run :: Options -> IO ()
run options = do
  lo <- setLogUseLoc False <$> logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          , appUserAgent = "ripper/" <> T.pack (showVersion Paths_ripper.version)
          }
     in runRIO app Ripper.Run.run

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
  <*> option duration
      ( short 'l'
      <> help "How long to rip (e.g. 2h)"
      <> metavar "rip_duration"
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

duration :: ReadM Duration
duration = eitherReader $ parseDuration . T.pack
