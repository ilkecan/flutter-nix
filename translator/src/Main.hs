module Main where

import Lock
  ( generateLockFile,
  )

main :: IO ()
main = do
  generateLockFile
