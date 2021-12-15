{-# LANGUAGE OverloadedStrings #-}

module Types.PubSpec
  ( PubSpec (PubSpec),
  )
where

import Data.Yaml

data PubSpec = PubSpec
  { name :: String,
    version :: String
  }
  deriving (Show)

instance FromJSON PubSpec where
  parseJSON = withObject "PubSpec" $ \spec ->
    PubSpec
      <$> spec .: "name"
      <*> spec .: "version"
