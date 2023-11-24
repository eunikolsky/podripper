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
import Ripper.RipperDelay

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
      <> help (mconcat ["How long to rip (e.g. ", show (durationHours 2), ")"])
      <> metavar "rip_duration"
      )
  <*> many (
        option ripIntervalRef
        ( short 'i'
        <> help "Time intervals with delays to extend the default delays"
        <> metavar "rip_intervals"
        )
      )
  <*> option retryDelay
      ( short 'r'
      <> help (mconcat ["Reconnect delay (e.g. ", show (RetryDelay $ durationMinutes 1), ")"])
      <> metavar "reconnect_delay"
      <> value (RetryDelay $ durationSeconds 5)
      <> showDefault
      )
  <*> option retryDelay
      ( short 's'
      <> help (mconcat ["Small reconnect delay (e.g. ", show (RetryDelay $ durationSeconds 5), ")"])
      <> metavar "reconnect_delay"
      <> value (RetryDelay $ durationSeconds 1)
      <> showDefault
      )
  <*> strArgument ( metavar "URL"
                <> help "Stream URL"
                  )

duration :: ReadM Duration
duration = eitherReader $ parseDuration . T.pack

retryDelay :: ReadM RetryDelay
retryDelay = RetryDelay <$> duration

ripIntervalRef :: ReadM RipperIntervalRef
ripIntervalRef = eitherReader $ parseRipperIntervalRef . T.pack
