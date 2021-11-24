module Main where

import Cli
  ( Options (Options),
    opts,
  )
import Lock
  ( generateLockFile,
  )
import Options.Applicative
  ( execParser,
  )

main :: IO ()
main = do
  Options pubspecLockFile flutterNixLockFile <- execParser opts
  generateLockFile pubspecLockFile flutterNixLockFile
