{-# LANGUAGE NoImplicitPrelude #-}

module Ripper.Util
  ( readCommand
  ) where

import RIO
import System.IO (putStrLn)
import UnliftIO.Process

readCommand :: String -> [String] -> String -> IO (Maybe String)
readCommand prog args input = do
  (code, out, err) <- readProcessWithExitCode prog args input
  if code == ExitSuccess
    then pure $ Just out
    else do
      putStrLn $ mconcat
        [ "readCommand ("
        , prog, " ", show args, " <<< ", input
        , "): exit code ", show code
        , "; stderr: ", err
        ]
      pure Nothing
