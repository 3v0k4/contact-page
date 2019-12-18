#!/usr/bin/env stack
{- stack
  script
  --resolver lts-14.17
  --package wreq
  --package optparse-applicative
  --package frontmatter
  --package aeson
  --package bytestring
  --package lens
  --package filepath
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Network.Wreq
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Frontmatter
import Data.Aeson
import GHC.Generics
import Data.ByteString
import Data.ByteString.Char8
import Control.Lens hiding ((.=))
import System.FilePath.Posix
import Data.Foldable

data Front =
  Front
    { title :: String
    , description :: String
    , tags :: [String]
    } deriving (Show, Generic, FromJSON)

data DevPost =
  DevPost
    { title :: String
    , description :: String
    , tags :: [String]
    , canonical_url :: String
    , published :: Bool
    , body_markdown :: String
    } deriving (Show, Generic)

instance ToJSON DevPost where
  toJSON DevPost{..} = object ["article" .= article]
    where article = object
            [ "title" .= title
            , "description" .= description
            , "tags" .= tags
            , "canonical_url" .= canonical_url
            , "published" .= published
            , "body_markdown" .= body_markdown
            ]

main :: IO ()
main = uncurry crosspost =<< execParser opts
  where
    opts = info (parser <**> helper)
      (  fullDesc
      <> progDesc "Crossposts POST to DevTo"
      )

parser :: Options.Applicative.Parser (String, String)
parser = (,)
    <$>
      Options.Applicative.argument str
        (  metavar "API_KEY"
        <> help "API_KEY to post on DevTo"
        )
    <*>
      Options.Applicative.argument str
        (  metavar "POST"
        <> help "Path to blog POST to post on DevTo"
        )

crosspost :: String -> String -> IO ()
crosspost apiKey path = do
  f <- Data.ByteString.readFile path
  case parseYamlFrontmatter f of
      Done postBS frontmatter -> do
        let post = Data.ByteString.Char8.unpack postBS
        let json = toJSON $ mkDevPost path frontmatter post
        let opts = defaults & Network.Wreq.header "api-key" .~ [Data.ByteString.Char8.pack apiKey]
                            & Network.Wreq.header "Content-Type" .~ [Data.ByteString.Char8.pack "application/json; charset=utf-8"]
        r <- Network.Wreq.postWith opts "https://dev.to/api/articles" json
        print $ r ^. responseStatus
      e ->
        error $ show e

mkDevPost :: String -> Front -> String -> DevPost
mkDevPost path Front{..} post = DevPost{..}
  where
    published = False
    body_markdown = post
    canonical_url = urlFor path

urlFor :: String -> String
urlFor path = fold [base, "/", name, ".html"]
  where
    base = "https://odone.io/posts"
    name = System.FilePath.Posix.takeBaseName path
