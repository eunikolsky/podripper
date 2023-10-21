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

  case progOptions of
    RunOptions ripName -> Podripper.run ripName
    RipperOptions options -> Ripper.run options
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
