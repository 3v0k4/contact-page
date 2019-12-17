{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( tweet
    ) where

import Data.Frontmatter
import Data.Yaml (Value)
import Data.ByteString
import Data.ByteString.Char8
import Data.Aeson
import GHC.Generics
import System.FilePath.Posix
import Web.Tweet
import Data.Foldable

data Front =
  Front
    { title :: String
    , description :: String
    } deriving (Show, Generic, FromJSON)

mkTweet :: FilePath -> Front -> String
mkTweet path Front{..} = fold [title, " 📒 ", description, ".\n\n", url]
  where
    base = "https://odone.io/posts"
    name = System.FilePath.Posix.takeBaseName path
    url = fold [base, "/", name, ".html"]

tweet :: String -> FilePath -> IO ()
tweet creds path =
  parseYamlFrontmatter <$> Data.ByteString.readFile path
    >>= \case
      Done _post frontmatter ->
        basicTweet (mkTweet path frontmatter) creds >> pure ()
      _ ->
        error "Parse failure"
