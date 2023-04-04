{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import qualified Main_RSSGen as RSSGen
import qualified Main_Ripper as Ripper
import Options.Applicative.Simple
import qualified Paths_ripper

main :: IO ()
main = do
  ((), progOptions) <- simpleOptions
    $(simpleVersion Paths_ripper.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (pure ()) $ do
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
    RipperOptions options -> Ripper.main options
    RSSGenOptions files -> RSSGen.main files

-- | Defines the options parsed either for the `ripper` or `rssgen` command.
-- This type is needed to combine the incompatible command options.
data ProgramOptions = RipperOptions Options | RSSGenOptions [FilePath]
