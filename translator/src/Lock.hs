module Lock
  ( generateLockFile,
  )
where

import Control.Concurrent.Async
  ( mapConcurrently,
  )
import Control.Monad.IO.Class
  ( liftIO,
  )
import Control.Monad.Trans.Except
  ( ExceptT (ExceptT),
    runExceptT,
    withExceptT,
  )
import Data.Aeson
  ( eitherDecodeFileStrict',
    encodeFile,
  )
import Data.Yaml
  ( decodeFileWithWarnings,
    prettyPrintParseException,
  )
import Prefetch.PubPackage
  ( prefetchPubPackage,
  )
import Prefetch.SdkDependency
  ( prefetchSdkDependency,
  )
import System.Environment
  ( getEnv,
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
import Types.SdkDependencies
  ( SdkDependencies (SdkDependencies),
    SdkDependency (SdkDependency),
    hash,
  )

getHostedPackages :: PubSpec -> IO [HostedPackage]
getHostedPackages (PubSpec pkgs) =
  mapConcurrently toHostedPackage [pkg | pkg@Hosted {} <- pkgs]

toHostedPackage :: PubPackage -> IO HostedPackage
toHostedPackage (Hosted name version url) = do
  hash' <- prefetchPubPackage name version url
  print hash'
  return $ HostedPackage name version url hash'
toHostedPackage _ = error "Can't create HostedPackage from PubPackage.Sdk"

getSdkPackages :: PubSpec -> [SdkPackage]
getSdkPackages (PubSpec pkgs) = [SdkPackage name | (Sdk name) <- pkgs]

getSdkDependencies :: SdkDependencies -> IO SdkDependencies
getSdkDependencies (SdkDependencies common android linux web) = do
  common' <- mapConcurrently getSdkDependency common
  android' <- mapConcurrently getSdkDependency android
  linux' <- mapConcurrently getSdkDependency linux
  web' <- mapConcurrently getSdkDependency web
  return $ SdkDependencies common' android' linux' web'

getSdkDependency :: SdkDependency -> IO SdkDependency
getSdkDependency dep@(SdkDependency name url stripRoot _) = do
  hash' <- prefetchSdkDependency name url stripRoot
  print hash'
  return $ dep {hash = hash'}

generateLockFile :: String -> String -> IO ()
generateLockFile pubspecLockFile flutterNixLockFile = do
  status <- runExceptT $ do
    (warnings, pubspec) <-
      withExceptT prettyPrintParseException $
        ExceptT (decodeFileWithWarnings pubspecLockFile)
    liftIO $ mapM_ print warnings

    hostedPackages <- liftIO $ getHostedPackages pubspec
    let sdkPackages = getSdkPackages pubspec

    sdkDependenciesJson <- liftIO $ getEnv "FLUTTER_SDK_DEPENDENCIES_JSON"
    sdkDependencies <- ExceptT $ eitherDecodeFileStrict' sdkDependenciesJson
    hashedSdkDependencies <- liftIO $ getSdkDependencies sdkDependencies

    let flutter2nix =
          FlutterNixLock
            hostedPackages
            sdkPackages
            hashedSdkDependencies
    liftIO $ encodeFile flutterNixLockFile flutter2nix

  case status of
    Left err -> print err
    Right _ -> return ()
