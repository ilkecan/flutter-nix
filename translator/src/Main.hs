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
  Options
    pubSpecFile
    pubSpecLockFile
    flutterNixLockFile
    noCache <-
    execParser opts
  generateLockFile pubSpecFile pubSpecLockFile flutterNixLockFile noCache
