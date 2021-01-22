#!/usr/bin/env stack
{- stack
 script
  --resolver lts-16.31
  --package composition-prelude
  --package htoml-megaparsec
  --package frontmatter
  --package yaml
  --package bytestring
  --package aeson
  --package text
  --package filepath
  --package tweet-hs
  --package optparse-applicative
  --package directory
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Aeson
import Data.ByteString
import Data.ByteString.Char8
import Data.Foldable
import Data.Frontmatter
import Data.List
import Data.Semigroup ((<>))
import Data.Text
import Data.Yaml (Value)
import GHC.Generics
import Options.Applicative
import System.Directory
import System.FilePath.Posix
import Web.Tweet

data Opts = Opts
  { creds :: String,
    post :: String
  }

data Front = Front
  { title :: Text,
    description :: Text,
    tags :: [Text]
  }
  deriving (Show, Generic, FromJSON)

main :: IO ()
main = do
  cs <- fmap (<> "/.cred.toml") getHomeDirectory
  execParser (opts cs) >>= (\Opts {..} -> tweet creds post)
  where
    opts cs =
      info
        (parser cs <**> helper)
        ( fullDesc
            <> progDesc "Shares on Twitter a new blog POST using CREDS for authentication"
        )

parser :: FilePath -> Options.Applicative.Parser Opts
parser creds =
  Opts
    <$> strOption
      ( long "creds"
          <> metavar "CREDS"
          <> help "Path to creds .toml file"
          <> value creds
          <> showDefault
      )
    <*> argument
      str
      ( metavar "POST"
          <> help "Path to blog post file"
      )

mkTweet :: FilePath -> Front -> String
mkTweet path Front {..} = Data.Text.unpack . fold $ [title, " 📒 ", description, "\n\n", htags, "\n\n", url]
  where
    base = "https://odone.io/posts"
    name = Data.Text.pack . System.FilePath.Posix.takeBaseName $ path
    url = fold [base, "/", name, ".html"]
    htags = Data.Text.unwords . fmap ((<>) "#" . replace " " "") $ tags

tweet :: String -> FilePath -> IO ()
tweet creds path =
  parseYamlFrontmatter <$> Data.ByteString.readFile path
    >>= \case
      Done _post frontmatter -> do
        let tweet = mkTweet path frontmatter
        Prelude.putStrLn tweet
      --basicTweet tweet creds >> pure ()
      e ->
        error $ show e
