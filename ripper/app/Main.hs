{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import qualified Main_Ripper as Ripper
import Options.Applicative.Simple
import qualified Paths_ripper

main :: IO ()
main = do
  ((), options) <- simpleOptions
    $(simpleVersion Paths_ripper.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (pure ()) $ do
      addCommand
        "ripper"
        "Rip a podcast live stream"
        id
        Ripper.ripperParser

  Ripper.main options
