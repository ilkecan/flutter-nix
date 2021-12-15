{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.FlutterNixLock
  ( FlutterNixLock (FlutterNixLock),
    HostedPackage (HostedPackage),
    SdkPackage (SdkPackage),
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON,
    Value,
    defaultOptions,
    genericToEncoding,
    object,
    pairs,
    parseJSON,
    toEncoding,
    toJSON,
    unwrapUnaryRecords,
    withObject,
    withText,
    (.:),
    (.=),
  )
import Data.Aeson.Encoding
  ( pair,
  )
import Data.Aeson.Types
  ( Parser,
  )
import Data.Map
  ( Map,
    lookup,
  )
import Data.Maybe
  ( fromJust,
  )
import Data.Text
  ( unpack,
  )
import GHC.Generics
  ( Generic,
  )
import Types.SdkDependencies
  ( SdkDependencies,
  )
import Prelude hiding
  ( lookup,
  )

data FlutterNixLock = FlutterNixLock
  { name :: String,
    version :: String,
    hostedPackages :: [HostedPackage],
    sdkPackages :: [SdkPackage],
    sdkDependencies :: SdkDependencies
  }
  deriving (Show)

instance ToJSON FlutterNixLock where
  toJSON (FlutterNixLock n v hostedPkgs sdkPkgs sdkDeps) =
    object
      [ "name" .= n,
        "version" .= v,
        "pubPackages"
          .= object
            [ "hosted" .= hostedPkgs,
              "sdk" .= sdkPkgs
            ],
        "sdkDependencies" .= sdkDeps
      ]

  toEncoding (FlutterNixLock n v hostedPkgs sdkPkgs sdkDeps) =
    pairs
      ( "name" .= n
          <> "version" .= v
          <> pair
            "pubPackages"
            ( pairs
                ( "hosted" .= hostedPkgs
                    <> "sdk" .= sdkPkgs
                )
            )
          <> "sdkDependencies" .= sdkDeps
      )

instance FromJSON FlutterNixLock where
  parseJSON = withObject "FlutterNixLock" $ \obj -> do
    pubPackages <- obj .: "pubPackages" :: Parser (Map String Value)
    let hosted = fromJust . lookup "hosted" $ pubPackages
    let sdk = fromJust . lookup "sdk" $ pubPackages
    FlutterNixLock
      <$> obj .: "name"
      <*> obj .: "version"
      <*> parseJSON hosted
      <*> parseJSON sdk
      <*> obj .: "sdkDependencies"

data HostedPackage = HostedPackage
  { name :: String,
    version :: String,
    url :: String,
    hash :: String
  }
  deriving (Show, Generic)

instance ToJSON HostedPackage where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON HostedPackage

newtype SdkPackage = SdkPackage
  { name :: String
  }
  deriving (Show, Generic)

instance ToJSON SdkPackage where
  toEncoding = genericToEncoding $ defaultOptions {unwrapUnaryRecords = True}

instance FromJSON SdkPackage where
  parseJSON = withText "SdkPackage" $ \pkg -> do
    return $ SdkPackage (unpack pkg)
