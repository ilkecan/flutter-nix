{-# LANGUAGE OverloadedStrings #-}

module Types.HashCache
  ( HashCache (HashCache),
    HostedPubPackageHashCache,
    SdkDependencyHashCaches (SdkDependencyHashCaches),
    SdkDependencyHashCache,
    emptyHashCache,
  )
where

import Data.Aeson
  ( FromJSON,
    Value,
    parseJSON,
    withArray,
    withObject,
    (.:),
  )
import Data.Aeson.Types
  ( Parser,
  )
import Data.HashMap.Strict as HM
  ( toList,
  )
import Data.Map
  ( Map,
    empty,
    fromList,
    lookup,
  )
import Data.Maybe
  ( fromJust,
  )
import Data.Text
  ( Text,
  )
import Data.Vector as V
  ( toList,
  )
import Prelude hiding
  ( lookup,
  )

data HashCache
  = HashCache
      HostedPubPackageHashCache
      SdkDependencyHashCaches

data SdkDependencyHashCaches = SdkDependencyHashCaches
  { _common :: SdkDependencyHashCache,
    _android :: SdkDependencyHashCache,
    _linux :: SdkDependencyHashCache,
    _web :: SdkDependencyHashCache
  }

emptyHashCache :: HashCache
emptyHashCache =
  HashCache empty $
    SdkDependencyHashCaches empty empty empty empty

type Hash = String

type HostedPubPackageKey = (String, String, String)

type HostedPubPackageHashCache = Map HostedPubPackageKey Hash

type SdkDependencyKey = String

type SdkDependencyHashCache = Map SdkDependencyKey Hash

instance FromJSON HashCache where
  parseJSON = withObject "FlutterNixLock" $ \obj -> do
    pubPackages <- obj .: "pubPackages" :: Parser (Map String Value)
    sdkDependencies <- obj .: "sdkDependencies" :: Parser (Map String Value)
    let hostedPubPackages = fromJust . lookup "hosted" $ pubPackages
        commonSdkDependencies = fromJust . lookup "common" $ sdkDependencies
        androidSdkDependencies = fromJust . lookup "android" $ sdkDependencies
        linuxSdkDependencies = fromJust . lookup "linux" $ sdkDependencies
        webSdkDependencies = fromJust . lookup "web" $ sdkDependencies

    hostedPubPackageHashCache <-
      withArray
        "hosted"
        (mapM parseHostedPubPackage . V.toList)
        hostedPubPackages

    commonSdkDependencyHashCache <-
      withObject
        "common"
        (mapM parseSdkDependency . HM.toList)
        commonSdkDependencies

    androidSdkDependencyHashCache <-
      withObject
        "android"
        (mapM parseSdkDependency . HM.toList)
        androidSdkDependencies

    linuxSdkDependencyHashCache <-
      withObject
        "linux"
        (mapM parseSdkDependency . HM.toList)
        linuxSdkDependencies

    webSdkDependencyHashCache <-
      withObject
        "web"
        (mapM parseSdkDependency . HM.toList)
        webSdkDependencies

    let sdkDependencyHashCaches =
          SdkDependencyHashCaches
            (fromList commonSdkDependencyHashCache)
            (fromList androidSdkDependencyHashCache)
            (fromList linuxSdkDependencyHashCache)
            (fromList webSdkDependencyHashCache)

    return $
      HashCache
        (fromList hostedPubPackageHashCache)
        sdkDependencyHashCaches

parseHostedPubPackage :: Value -> Parser (HostedPubPackageKey, Hash)
parseHostedPubPackage = withObject "HostedPubPackage" $ \pkg -> do
  name <- pkg .: "name"
  version <- pkg .: "version"
  url <- pkg .: "url"
  hash <- pkg .: "hash"
  return ((name, version, url), hash)

parseSdkDependency :: (Text, Value) -> Parser (SdkDependencyKey, Hash)
parseSdkDependency (_key, value) =
  withObject
    "SdkDependency"
    ( \dep -> do
        url <- dep .: "url" :: Parser String
        hash <- dep .: "hash" :: Parser String
        return (url, hash)
    )
    value
