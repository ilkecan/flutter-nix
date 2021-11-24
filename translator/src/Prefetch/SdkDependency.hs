module Prefetch.SdkDependency
  ( prefetchSdkDependency,
  )
where

import Data.List.Extra
  ( dropEnd,
  )
import System.Process
  ( readProcess,
  )

prefetchSdkDependency :: String -> String -> Bool -> IO String
prefetchSdkDependency name url stripRoot = do
  stdout <-
    readProcess
      "nix-prefetch"
      [ "fetchzip",
        "--name",
        name,
        "--url",
        url,
        if stripRoot then "--stripRoot" else "--no-stripRoot",
        "--silent"
      ]
      ""
  return $ dropEnd 1 stdout
