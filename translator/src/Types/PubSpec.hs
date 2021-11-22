{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.PubSpec
  ( PubSpec (PubSpec),
    PubPackage (Hosted, Sdk),
  )
where

import Data.Map
  ( Map,
    toList,
  )
import Data.Yaml hiding
  ( Value,
  )
import qualified Data.Yaml as Yaml
  ( Value,
  )

newtype PubSpec = PubSpec
  { packages :: [PubPackage]
  }
  deriving (Show)

instance FromJSON PubSpec where
  parseJSON = withObject "PubSpec" $ \spec -> do
    pkgs <- spec .: "packages" :: Parser (Map String Yaml.Value)
    PubSpec <$> mapM parsePubPackage (toList pkgs)

data PubPackage
  = Hosted
      { name :: !String,
        version :: !String,
        url :: !String
      }
  | Sdk
      { name :: !String
      }
  deriving (Show)

parsePubPackage :: (String, Yaml.Value) -> Parser PubPackage
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
