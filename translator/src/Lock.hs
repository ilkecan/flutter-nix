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
  )
import Data.Aeson
  ( eitherDecodeFileStrict',
    encodeFile,
  )
import Data.Yaml
  ( decodeFileThrow,
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
  ( PubSpec (PubSpec),
  )
import Types.PubSpecLock
  ( PubPackage (Hosted, Sdk),
    PubSpecLock (PubSpecLock),
  )
import Types.SdkDependencies
  ( SdkDependencies (SdkDependencies),
    SdkDependency (SdkDependency),
    hash,
  )

getHostedPackages :: PubSpecLock -> IO [HostedPackage]
getHostedPackages (PubSpecLock pkgs) =
  mapConcurrently toHostedPackage [pkg | pkg@Hosted {} <- pkgs]

toHostedPackage :: PubPackage -> IO HostedPackage
toHostedPackage (Hosted name version url) = do
  hash' <- prefetchPubPackage name version url
  print hash'
  return $ HostedPackage name version url hash'
toHostedPackage _ = error "Can't create HostedPackage from PubPackage.Sdk"

getSdkPackages :: PubSpecLock -> [SdkPackage]
getSdkPackages (PubSpecLock pkgs) = [SdkPackage name | (Sdk name) <- pkgs]

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

generateLockFile :: String -> String -> String -> IO ()
generateLockFile pubSpecFile pubSpecLockFile flutterNixLockFile = do
  status <- runExceptT $ do
    PubSpec name version <- liftIO $ decodeFileThrow pubSpecFile
    pubSpecLock <- liftIO $ decodeFileThrow pubSpecLockFile

    hostedPackages <- liftIO $ getHostedPackages pubSpecLock
    let sdkPackages = getSdkPackages pubSpecLock

    sdkDependenciesJson <- liftIO $ getEnv "FLUTTER_SDK_DEPENDENCIES_JSON"
    sdkDependencies <- ExceptT $ eitherDecodeFileStrict' sdkDependenciesJson
    hashedSdkDependencies <- liftIO $ getSdkDependencies sdkDependencies

    let flutter2nix =
          FlutterNixLock
            name
            version
            hostedPackages
            sdkPackages
            hashedSdkDependencies
    liftIO $ encodeFile flutterNixLockFile flutter2nix

  case status of
    Left err -> print err
    Right _ -> return ()
