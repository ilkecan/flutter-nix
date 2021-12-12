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
  ( ToJSON,
    defaultOptions,
    genericToEncoding,
    object,
    pairs,
    toEncoding,
    toJSON,
    unwrapUnaryRecords,
    (.=),
  )
import Data.Aeson.Encoding
  ( pair,
  )
import GHC.Generics
  ( Generic,
  )
import Types.SdkDependencies
  ( SdkDependencies,
  )

data FlutterNixLock = FlutterNixLock
  { name :: !String,
    version :: !String,
    hostedPackages :: ![HostedPackage],
    sdkPackages :: ![SdkPackage],
    sdkDependencies :: !SdkDependencies
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

data HostedPackage = HostedPackage
  { name :: !String,
    version :: !String,
    url :: !String,
    hash :: !String
  }
  deriving (Show, Generic)

instance ToJSON HostedPackage where
  toEncoding = genericToEncoding defaultOptions

newtype SdkPackage = SdkPackage
  { name :: String
  }
  deriving (Show, Generic)

instance ToJSON SdkPackage where
  toEncoding = genericToEncoding $ defaultOptions {unwrapUnaryRecords = True}
