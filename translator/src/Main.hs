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
  Options pubSpecFile pubSpecLockFile flutterNixLockFile <- execParser opts
  generateLockFile pubSpecFile pubSpecLockFile flutterNixLockFile
