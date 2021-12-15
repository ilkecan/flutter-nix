{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.SdkDependencies
  ( SdkDependencies (SdkDependencies),
    SdkDependency (SdkDependency),
    hash,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON,
    object,
    parseJSON,
    toJSON,
    withObject,
    (.:),
    (.=),
  )
import Data.Text
  ( pack,
  )
import GHC.Generics
  ( Generic,
  )

data SdkDependencies = SdkDependencies
  { common :: [SdkDependency],
    android :: [SdkDependency],
    linux :: [SdkDependency],
    web :: [SdkDependency]
  }
  deriving (Show, Generic)

instance ToJSON SdkDependencies

instance FromJSON SdkDependencies

data SdkDependency = SdkDependency
  { name :: String,
    url :: String,
    stripRoot :: Bool,
    hash :: String
  }
  deriving (Show)

instance FromJSON SdkDependency where
  parseJSON = withObject "SdkDependency" $ \dep -> do
    SdkDependency
      <$> dep .: "name"
      <*> dep .: "url"
      <*> dep .: "stripRoot"
      <*> return ""

-- TODO: I don't like this. Why does `toJSONList` not work?
instance {-# OVERLAPPING #-} ToJSON [SdkDependency] where
  toJSON deps =
    object $ map toJson deps
    where
      toJson (SdkDependency name' url' stripRoot' hash') =
        pack name'
          .= object
            [ "url" .= url',
              "stripRoot" .= stripRoot',
              "hash" .= hash'
            ]
