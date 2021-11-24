{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.SdkDependency
  ( SdkDependency (SdkDependency),
    hash,
  )
where

import Data.Aeson
import Data.Text
  ( pack,
  )

data SdkDependency = SdkDependency
  { name :: !String,
    url :: !String,
    stripRoot :: !Bool,
    hash :: !String
  }
  deriving (Show)

instance FromJSON SdkDependency where
  parseJSON = withObject "SdkDependency" $ \dep -> do
    SdkDependency
      <$> dep .: "name"
      <*> dep .: "url"
      <*> dep .: "stripRoot"
      <*> return ""

-- TODO: I don't like this. Why `toJSONList` doesn't work?
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
