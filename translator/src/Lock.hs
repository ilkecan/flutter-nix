module Lock
  ( lock,
  )
where

import Data.Aeson
  ( encodeFile,
  )
import Data.Yaml
  ( decodeFileWithWarnings,
  )
import Prefetch
  ( prefetch,
  )
import Types.FlutterNixLock
  ( FlutterNixLock (FlutterNixLock),
    HostedPackage (HostedPackage),
    SdkPackage (SdkPackage),
  )
import Types.PubSpec
  ( PubPackage (Hosted, Sdk),
    PubSpec (PubSpec),
  )

getHostedPackages :: PubSpec -> IO [HostedPackage]
getHostedPackages (PubSpec pkgs) =
  -- try parallelizing this
  mapM toHostedPackage [pkg | pkg@Hosted {} <- pkgs]

toHostedPackage :: PubPackage -> IO HostedPackage
toHostedPackage (Hosted name version url) = do
  hash <- prefetch name version url
  print hash
  return $ HostedPackage name version url hash
toHostedPackage _ = error "Can't create HostedPackage from PubPackage.Sdk"

getSdkPackages :: PubSpec -> [SdkPackage]
getSdkPackages (PubSpec pkgs) = [SdkPackage name | (Sdk name) <- pkgs]

lock :: String -> String -> IO ()
lock pubspecLockFile flutterNixLockFile = do
  pubspec <- decodeFileWithWarnings pubspecLockFile
  case pubspec of
    Right (ws, v) -> do
      mapM_ print ws

      hostedPackages <- getHostedPackages v
      let sdkPackages = getSdkPackages v
      let flutter2nix = FlutterNixLock hostedPackages sdkPackages
      encodeFile flutterNixLockFile flutter2nix
    Left e -> print e
