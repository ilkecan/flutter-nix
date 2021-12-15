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
import Data.Map
  ( lookup,
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
import Types.HashCache
  ( HashCache (HashCache),
    HostedPubPackageHashCache,
    SdkDependencyHashCache,
    SdkDependencyHashCaches (SdkDependencyHashCaches),
    emptyHashCache,
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
import Prelude hiding
  ( lookup,
  )

getHostedPackages ::
  HostedPubPackageHashCache ->
  PubSpecLock ->
  IO [HostedPackage]
getHostedPackages cache (PubSpecLock pkgs) =
  mapConcurrently (toHostedPackage cache) [pkg | pkg@Hosted {} <- pkgs]

toHostedPackage :: HostedPubPackageHashCache -> PubPackage -> IO HostedPackage
toHostedPackage cache (Hosted name version url) = do
  case lookup (name, version, url) cache of
    Just hash' -> do
      let pkg = HostedPackage name version url hash'
      putStrLn $ "The resource is found in the old lock file: " ++ show pkg
      return pkg
    Nothing -> do
      hash' <- prefetchPubPackage name version url
      let pkg = HostedPackage name version url hash'
      putStrLn $ "The resource is prefetched: " ++ show pkg
      return pkg
toHostedPackage _ _ = error "Can't create HostedPackage from PubPackage.Sdk"

getSdkPackages :: PubSpecLock -> [SdkPackage]
getSdkPackages (PubSpecLock pkgs) = [SdkPackage name | (Sdk name) <- pkgs]

getSdkDependencies ::
  SdkDependencyHashCaches ->
  SdkDependencies ->
  IO SdkDependencies
getSdkDependencies
  (SdkDependencyHashCaches commonCache androidCache linuxCache webCache)
  (SdkDependencies common android linux web) =
    do
      common' <- mapConcurrently (getSdkDependency commonCache) common
      android' <- mapConcurrently (getSdkDependency androidCache) android
      linux' <- mapConcurrently (getSdkDependency linuxCache) linux
      web' <- mapConcurrently (getSdkDependency webCache) web
      return $ SdkDependencies common' android' linux' web'

getSdkDependency :: SdkDependencyHashCache -> SdkDependency -> IO SdkDependency
getSdkDependency cache dep@(SdkDependency name url stripRoot _) = do
  case lookup url cache of
    Just hash' -> do
      let pkg = dep {hash = hash'}
      putStrLn $ "The resource is found in the old lock file: " ++ show pkg
      return pkg
    Nothing -> do
      hash' <- prefetchSdkDependency name url stripRoot
      let pkg = dep {hash = hash'}
      putStrLn $ "The resource is prefetched: " ++ show pkg
      return pkg

getHashCache :: String -> IO HashCache
getHashCache file = do
  result <- eitherDecodeFileStrict' file
  case result of
    Left _ -> do
      putStrLn $ "The old lock file (" ++ file ++ ") be used as a cache."
      return emptyHashCache
    Right hashCache -> do
      putStrLn $ "The old lock file (" ++ file ++ ") will be used as a cache."
      return hashCache

generateLockFile :: String -> String -> String -> IO ()
generateLockFile pubSpecFile pubSpecLockFile flutterNixLockFile =
  do
    runExceptT $ do
      HashCache hostedPubPackageHashCache sdkDependencyHashCaches <-
        liftIO $ getHashCache flutterNixLockFile

      PubSpec name version <- liftIO $ decodeFileThrow pubSpecFile
      pubSpecLock <- liftIO $ decodeFileThrow pubSpecLockFile

      hostedPackages <-
        liftIO $ getHostedPackages hostedPubPackageHashCache pubSpecLock
      let sdkPackages = getSdkPackages pubSpecLock

      sdkDependenciesJson <- liftIO $ getEnv "FLUTTER_SDK_DEPENDENCIES_JSON"
      sdkDependencies <- ExceptT $ eitherDecodeFileStrict' sdkDependenciesJson
      hashedSdkDependencies <-
        liftIO $ getSdkDependencies sdkDependencyHashCaches sdkDependencies

      let flutter2nix =
            FlutterNixLock
              name
              version
              hostedPackages
              sdkPackages
              hashedSdkDependencies
      liftIO $ encodeFile flutterNixLockFile flutter2nix
    >>= either print return
