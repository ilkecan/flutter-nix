module Prefetch
  ( prefetch,
  )
where

import Data.List.Extra
  ( dropEnd,
  )
import Paths_translator
  ( getDataFileName,
  )
import System.Process
  ( readProcess,
  )

prefetch :: String -> String -> String -> IO String
prefetch name version url = do
  fetcherFile <- getDataFileName "fetch-pub.nix"
  print fetcherFile
  stdout <-
    readProcess
      "nix-prefetch"
      [ "-f",
        fetcherFile,
        "--name",
        name,
        "--version",
        version,
        "--url",
        url,
        "--silent"
      ]
      ""
  return $ dropEnd 1 stdout
