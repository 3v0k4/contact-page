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
  --package lens-aeson
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
import Data.Aeson.Lens (_String, key)
import Data.ByteString
import Data.ByteString.Char8
import Data.Foldable
import Data.Frontmatter
import Data.Semigroup ((<>))
import Data.Text
import Data.Text.Encoding
import GHC.Generics
import Network.Wreq
import Options.Applicative
import System.FilePath.Posix

data Front
  = Front
      { title :: Text,
        description :: Text,
        tags :: [Text]
      }
  deriving (Show, Generic, FromJSON)

data MediumPost
  = MediumPost
      { title :: Text,
        tags :: [Text],
        canonicalUrl :: Text,
        publishStatus :: Text,
        content :: Text,
        contentFormat :: Text,
        notifyFollowers :: Bool
      }
  deriving (Show, Generic)

instance ToJSON MediumPost where
  toJSON MediumPost {..} =
    object
      [ "title" .= title,
        "tags" .= tags,
        "canonicalUrl" .= canonicalUrl,
        "publishStatus" .= publishStatus,
        "content" .= content,
        "contentFormat" .= contentFormat,
        "notifyFollowers" .= notifyFollowers
      ]

main :: IO ()
main = uncurry crosspost =<< execParser opts
  where
    opts =
      info
        (parser <**> helper)
        ( fullDesc
            <> progDesc "Crossposts POST to Medium"
        )

parser :: Options.Applicative.Parser (Text, Text)
parser =
  (,)
    <$> Options.Applicative.argument
      str
      ( metavar "API_KEY"
          <> help "API_KEY to post on Medium"
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
      let opts =
            defaults
              & Network.Wreq.auth ?~ Network.Wreq.oauth2Bearer (encodeUtf8 apiKey)
      r <- Network.Wreq.getWith opts "https://api.medium.com/v1/me"
      let id_ = r ^. responseBody . key "data" . key "id" . _String
      let post = decodeUtf8 postBS
      let json = toJSON $ mkMediumPost path frontmatter post
      let opts2 =
            defaults
              & Network.Wreq.auth ?~ Network.Wreq.oauth2Bearer (encodeUtf8 apiKey)
              & Network.Wreq.header "Content-Type" .~ [encodeUtf8 "application/json; charset=utf-8"]
      r2 <-
        Network.Wreq.postWith
          opts
          ("https://api.medium.com/v1/users/" <> Data.Text.unpack id_ <> "/posts")
          json
      print $ r2 ^? responseBody
    e ->
      error $ show e

mkMediumPost :: Text -> Front -> Text -> MediumPost
mkMediumPost path Front {..} post = MediumPost {..}
  where
    publishStatus = "draft"
    content = fold [preText canonicalUrl, post, postText]
    contentFormat = "markdown"
    canonicalUrl = urlFor path
    notifyFollowers = True

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
