module Main where

import Cli
  ( Options (Options),
    opts,
  )
import Lock
  ( lock,
  )
import Options.Applicative

main :: IO ()
main = do
  Options pubspecLockFile flutterNixLockFile <- execParser opts
  lock pubspecLockFile flutterNixLockFile
