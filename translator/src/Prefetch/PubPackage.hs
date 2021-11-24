module Prefetch.PubPackage
  ( prefetchPubPackage,
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

prefetchPubPackage :: String -> String -> String -> IO String
prefetchPubPackage name version url = do
  fetcherFile <- getDataFileName "fetch-pub.nix"
  stdout <-
    readProcess
      "nix-prefetch"
      [ "--file",
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
