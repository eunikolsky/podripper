module Ripper.Main
  ( ripperParser
  , run
  ) where

import Data.List (intercalate)
import Data.Time
import Data.Version (showVersion)
import RSSGen.Duration
import Ripper.Import
import Options.Applicative
import qualified Paths_ripper
import RIO.Process
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (splitOn)
import qualified Ripper.Run
import Ripper.RipperDelay

run :: Options -> RipsQueue -> IO ()
run options ripsQueue = do
  lo <- setLogUseLoc False <$> logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          , appUserAgent = "ripper/" <> T.pack (showVersion Paths_ripper.version)
          , appRipsQueue = ripsQueue
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
  <*> (Just <$> option duration
        ( short 'l'
        <> help (mconcat ["How long to rip (e.g. ", show (durationHours 2), ")"])
        <> metavar "rip_duration"
        )
      )
  <*> option (listParserOpt parseRipperIntervalRef)
      ( short 'i'
      <> help (mconcat
          [ "Comma-separated time intervals with delays to override the default delays (e.g. "
          , show (RipperIntervalRef Sunday (read "12:59:00", read "23:48:00") "America/New_York" (RetryDelay $ durationMinutes 9))
          , ")"
          ]
         )
      <> metavar "rip_intervals"
      <> value []
      <> showDefaultWith asList
      )
  <*> option (listParserOpt parsePostRipEndDelay)
      ( long "postripdelay"
      <> help "Comma-separated delays to be used after a rip has ended"
      <> value
          [ PostRipEndDelay (durationMinutes 5) (RetryDelay $ durationSeconds 1)
          , PostRipEndDelay (durationMinutes 15) (RetryDelay $ durationSeconds 3)
          ]
      <> showDefaultWith asList
      )
  <*> option retryDelay
      ( long "defdelay"
      <> help "The default ripper delay when no other rules match"
      <> value (RetryDelay $ durationMinutes 10)
      <> showDefault
      )
  <*> argument (SimpleURL . StreamURL . URL <$> str)
      ( metavar "URL"
      <> help "Stream URL"
      )

duration :: ReadM Duration
duration = eitherReader $ parseDuration . T.pack

retryDelay :: ReadM RetryDelay
retryDelay = RetryDelay <$> duration

listParserOpt :: (Text -> Either String a) -> ReadM [a]
listParserOpt a = eitherReader $ traverse a . T.splitOn "," . T.pack

asList :: Show a => [a] -> String
asList [] = "<empty>"
asList xs = intercalate "," $ show <$> xs
