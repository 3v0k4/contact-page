#!/usr/bin/env stack
{- stack
  script
  --resolver lts-16.31
  --package wreq
  --package optparse-applicative
  --package frontmatter
  --package aeson
  --package bytestring
  --package lens
  --package filepath
  --package text
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.ByteString
import Data.ByteString.Char8
import Data.Foldable
import Data.Frontmatter
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Text
import Data.Text.Encoding
import GHC.Generics
import Network.Wreq
import Options.Applicative
import System.FilePath.Posix

data Front = Front
  { title :: Text,
    description :: Text,
    tags :: [Text],
    cover_image :: Maybe Text
  }
  deriving (Show, Generic, FromJSON)

data DevPost = DevPost
  { title :: Text,
    description :: Text,
    tags :: [Text],
    canonical_url :: Text,
    published :: Bool,
    body_markdown :: Text,
    cover_image :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON DevPost where
  toJSON DevPost {..} = object ["article" .= article]
    where
      article =
        object
          [ "title" .= title,
            "description" .= description,
            "tags" .= tags,
            "canonical_url" .= canonical_url,
            "published" .= published,
            "body_markdown" .= body_markdown,
            "cover_image" .= fromMaybe "" cover_image
          ]

main :: IO ()
main = uncurry crosspost =<< execParser opts
  where
    opts =
      info
        (parser <**> helper)
        ( fullDesc
            <> progDesc "Crossposts POST to DevTo"
        )

parser :: Options.Applicative.Parser (Text, Text)
parser =
  (,)
    <$> Options.Applicative.argument
      str
      ( metavar "API_KEY"
          <> help "API_KEY to post on DevTo"
      )
    <*> Options.Applicative.argument
      str
      ( metavar "POST"
          <> help "Path to blog POST to crosspost"
      )

crosspost :: Text -> Text -> IO ()
crosspost apiKey path = do
  f <- Data.ByteString.readFile . Data.Text.unpack $ path
  case parseYamlFrontmatter f of
    Done postBS frontmatter -> do
      let post = decodeUtf8 postBS
      let json = toJSON $ mkDevPost path frontmatter post
      let opts =
            defaults & Network.Wreq.header "api-key" .~ [encodeUtf8 apiKey]
              & Network.Wreq.header "Content-Type" .~ [encodeUtf8 "application/json; charset=utf-8"]
      r <- Network.Wreq.postWith opts "https://dev.to/api/articles" json
      print $ r ^. responseStatus
    e ->
      error $ show e

mkDevPost :: Text -> Front -> Text -> DevPost
mkDevPost path Front {..} post = devPost {tags = replace " " "" <$> tags}
  where
    published = False
    canonical_url = urlFor path
    body_markdown = fold [preText canonical_url, post, postText]
    devPost = DevPost {..}

preText :: Text -> Text
preText url =
  fold
    [ "You can keep reading here or [jump to my blog](",
      url,
      ") to get the full experience, including the wonderful pink, blue and white palette.\n\n---\n\n"
    ]

postText :: Text
postText =
  "\n\n---\n\nGet the latest content via email from me personally. Reply with your thoughts. Let's learn from each other. Subscribe to my [PinkLetter](https://odone.io#newsletter)!"

urlFor :: Text -> Text
urlFor path = fold [base, "/", name, ".html"]
  where
    base = "https://odone.io/posts"
    name = Data.Text.pack . System.FilePath.Posix.takeBaseName . Data.Text.unpack $ path
