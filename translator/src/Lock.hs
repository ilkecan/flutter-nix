module Lock
  ( generateLockFile,
  )
where

import Cli
  ( flutterNixLockFile,
    noCache,
    pubSpecFile,
    pubSpecLockFile,
  )
import Control.Concurrent.Async
  ( mapConcurrently,
  )
import Control.Exception
  ( catch,
    throwIO,
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
  )
import Data.Aeson.Encode.Pretty
  ( Indent (Spaces),
    confCompare,
    confIndent,
    confTrailingNewline,
    defConfig,
    encodePretty',
  )
import Data.ByteString.Lazy
  ( writeFile,
  )
import Data.Map
  ( lookup,
  )
import Data.Yaml
  ( decodeFileThrow,
  )
import Log
  ( logDebug,
    logError,
    logInfo,
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
import System.IO.Error
  ( isDoesNotExistError,
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
    writeFile,
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
      logDebug $ "The resource is found in the old lock file: " <> show pkg
      return pkg
    Nothing -> do
      hash' <- prefetchPubPackage name version url
      let pkg = HostedPackage name version url hash'
      logDebug $ "The resource is prefetched: " <> show pkg
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
      logDebug $ "The resource is found in the old lock file: " <> show pkg
      return pkg
    Nothing -> do
      hash' <- prefetchSdkDependency name url stripRoot
      let pkg = dep {hash = hash'}
      logDebug $ "The resource is prefetched: " <> show pkg
      return pkg

getHashCache :: IO HashCache
getHashCache = do
  noCache' <- noCache
  file <- flutterNixLockFile
  if noCache'
    then do
      logInfo $ "The old lock file (" <> file <> ") won't be used as cache."
      return emptyHashCache
    else do
      let handleDoesNotExist e
            | isDoesNotExistError e = return $ Left ""
            | otherwise = throwIO e
      result <- eitherDecodeFileStrict' file `catch` handleDoesNotExist
      case result of
        Left _ -> do
          logInfo $
            "The old lock file (" <> file <> ") can't be used as cache."
          return emptyHashCache
        Right hashCache -> do
          logInfo $
            "The old lock file (" <> file <> ") will be used as cache."
          return hashCache

generateLockFile :: IO ()
generateLockFile =
  do
    runExceptT $ do
      HashCache hostedPubPackageHashCache sdkDependencyHashCaches <-
        liftIO getHashCache

      PubSpec name version <- liftIO $ decodeFileThrow =<< pubSpecFile
      pubSpecLock <- liftIO $ decodeFileThrow =<< pubSpecLockFile

      hostedPackages <-
        liftIO $ getHostedPackages hostedPubPackageHashCache pubSpecLock
      let sdkPackages = getSdkPackages pubSpecLock

      sdkDependenciesJson <- liftIO $ getEnv "FLUTTER_SDK_DEPENDENCIES_JSON"
      sdkDependencies <- ExceptT $ eitherDecodeFileStrict' sdkDependenciesJson
      hashedSdkDependencies <-
        liftIO $ getSdkDependencies sdkDependencyHashCaches sdkDependencies

      let flutterNix =
            FlutterNixLock
              name
              version
              hostedPackages
              sdkPackages
              hashedSdkDependencies
          encodeConfig =
            defConfig
              { confCompare = compare,
                confIndent = Spaces 2,
                confTrailingNewline = True
              }
          flutterNixEncoded = encodePretty' encodeConfig flutterNix

      flutterNixLockFile' <- liftIO flutterNixLockFile
      liftIO $ writeFile flutterNixLockFile' flutterNixEncoded
    >>= either logError return
