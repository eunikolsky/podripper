{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Data.Version (showVersion)
import qualified Main_RSSGen as RSSGen
import qualified Main_Ripper as Ripper
import qualified Main_Run as Run
import Options.Applicative.Simple
import RIO
import RIO.Text qualified as T
import Ripper.Types
import qualified Paths_ripper
import System.Exit (die)

main :: IO ()
main = do
  ((), progOptions) <- simpleOptions
    (showVersion Paths_ripper.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (pure ()) $ do
      addCommand
        "run"
        "Run the program"
        RunOptions
        Run.runParser

      addCommand
        "ripper"
        "Rip a podcast live stream"
        RipperOptions
        Ripper.ripperParser

      addCommand
        "rssgen"
        "Generate the RSS for the podcast"
        RSSGenOptions
        RSSGen.rssGenParser

  case progOptions of
    RunOptions ripName -> die $ mconcat ["TODO implement ripping ", T.unpack ripName, "!"]
    RipperOptions options -> Ripper.main options
    RSSGenOptions files -> RSSGen.main files

-- | Defines the options parsed for the request command.
-- This type is needed to combine the incompatible command options.
data ProgramOptions
  = RunOptions Run.RipName
  | RipperOptions Options
  | RSSGenOptions (NonEmpty FilePath)
