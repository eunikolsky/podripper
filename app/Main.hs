{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Data.Version (showVersion)
import Options.Applicative
import qualified Podripper
import RIO
import qualified RSSGen.Main as RSSGen
import qualified Ripper.Main as Ripper
import Ripper.Types
import qualified Run
import qualified Paths_ripper

main :: IO ()
main = do
  let version = showVersion Paths_ripper.version
      opts = info (programOptions <**> simpleVersioner version <**> helper)
        ( fullDesc
        <> header "Header for command line arguments"
        <> progDesc "Program description, also for command line arguments"
        )
  progOptions <- execParser opts

  setLineBuffering

  case progOptions of
    RunOptions ripName -> Podripper.run ripName
    RipperOptions options -> atomically newTQueue >>= Ripper.run options
    RSSGenOptions files -> RSSGen.run files

programOptions :: Parser ProgramOptions
programOptions = hsubparser $ mconcat
  [ cmd "run" "Run the program" RunOptions Run.runParser
  , cmd "ripper" "Rip a podcast live stream" RipperOptions Ripper.ripperParser
  , cmd "rssgen" "Generate the RSS for the podcast" RSSGenOptions RSSGen.rssGenParser
  ]

  where
    cmd name desc constr parser = command name $
      info (constr <$> parser) (progDesc desc)

-- | Defines the options parsed for the request command.
-- This type is needed to combine the incompatible command options.
data ProgramOptions
  = RunOptions Run.RipName
  | RipperOptions Options
  | RSSGenOptions (NonEmpty FilePath)

-- | Sets the line buffering mode for `stdout` and `stderr` to make sure log
-- lines are displayed promptly and in order.
--
-- The issue is that podripper's logs weren't always in the correct time
-- sequence — this only happened when running via `systemd`. For radiot and
-- rcmp, there were first ripper's logs, then the wrapper's (podripper's) logs,
-- that is the "starting the ripper" message appeared after the multi-hour
-- ripper was done; for atp, the podripper's logs did appear first, but only in
-- blocks (visible by systemd's timestamps), and they were interrupted by the
-- entirety of the ripper's logs. ripper uses a proper logger into `stderr`,
-- whereas podripper uses plain logging to `stdout`.
--
-- See also: https://stackoverflow.com/questions/19520885/haskell-default-io-buffering.
--
-- FIXME use a proper logger for the entire program
setLineBuffering :: IO ()
setLineBuffering = forM_ [stdout, stderr] $ flip hSetBuffering LineBuffering
