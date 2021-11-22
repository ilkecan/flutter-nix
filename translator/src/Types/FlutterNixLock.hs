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
    genericToJSON,
    object,
    pairs,
    toEncoding,
    toJSON,
    (.=),
  )
import Data.Aeson.Encoding
  ( pair,
  )
import GHC.Generics
  ( Generic,
  )

data FlutterNixLock = FlutterNixLock
  { hosted :: ![HostedPackage],
    sdk :: ![SdkPackage]
  }
  deriving (Show, Generic)

instance ToJSON FlutterNixLock where
  toJSON v = object ["packages" .= genericToJSON defaultOptions v]
  toEncoding = pairs . pair "packages" . genericToEncoding defaultOptions

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
  toEncoding = genericToEncoding defaultOptions
