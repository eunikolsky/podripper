{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Data.Version (showVersion)
import Import
import Run
import RIO.Process
import qualified RIO.Text as T
import Options.Applicative.Simple
import qualified Paths_ripper

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_ripper.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
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
    )
    empty
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