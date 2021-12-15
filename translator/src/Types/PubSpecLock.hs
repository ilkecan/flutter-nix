{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.PubSpecLock
  ( PubSpecLock (PubSpecLock),
    PubPackage (Hosted, Sdk),
  )
where

import Data.Map
  ( Map,
    toList,
  )
import Data.Yaml
  ( FromJSON,
    Parser,
    Value,
    parseJSON,
    withObject,
    (.:),
  )

newtype PubSpecLock = PubSpecLock
  { packages :: [PubPackage]
  }
  deriving (Show)

instance FromJSON PubSpecLock where
  parseJSON = withObject "PubSpecLock" $ \specLock -> do
    pkgs <- specLock .: "packages" :: Parser (Map String Value)
    PubSpecLock <$> mapM parsePubPackage (toList pkgs)

data PubPackage
  = Hosted
      { name :: String,
        version :: String,
        url :: String
      }
  | Sdk
      { name :: String
      }
  deriving (Show)

parsePubPackage :: (String, Value) -> Parser PubPackage
parsePubPackage (key, value) =
  withObject
    "PubPackage"
    ( \pkg -> do
        source <- pkg .: "source" :: Parser String
        case source of
          "hosted" -> do
            desc <- pkg .: "description"
            Hosted <$> desc .: "name" <*> pkg .: "version" <*> desc .: "url"
          "sdk" -> return $ Sdk key
          _ ->
            error
              "'source' property of a package should either be hosted or sdk."
    )
    value
