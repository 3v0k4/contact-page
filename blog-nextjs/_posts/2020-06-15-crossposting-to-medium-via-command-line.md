---
title: Crossposting to Medium via Command Line
description: Using a Haskell script to crosspost to Medium a blog post with a frontmatter as used in Jekyll markdown files
published: true
author: Riccardo
tags:
  - Functional Programming
  - Haskell
  - Script
---

This post is heavily based on ["Crossposting to DevTo via Command Line"](https://odone.io/posts/2020-01-13-crossposting-via-command-line.html). However, that is not a prerequisite reading.

Let's see how to crosspost a local Jekyll-like blog post to [Medium](https://medium.com). First, we have the `crosspost` function:

```hs
crosspost :: Text -> Text -> IO ()
crosspost apiKey path = do
--        ^ Medium API key.
--               ^ Filepath to the blog post to crosspost.
  f <- Data.ByteString.readFile . Data.Text.unpack $ path
  case parseYamlFrontmatter f of
--     ^ Parse the frontmatter (and content) of the blog post to crosspost.
    Done postBS frontmatter -> do
--  ^ If the frontmatter (and content) was parsed successfully..
      let opts =
            defaults
              & Network.Wreq.auth ?~ Network.Wreq.oauth2Bearer (encodeUtf8 apiKey)
      r <- Network.Wreq.getWith opts "https://api.medium.com/v1/me"
      let id_ = r ^. responseBody . key "data" . key "id" . _String
--        ^ ..then get the user id associated with the API key and..

      let post = decodeUtf8 postBS
      let json = toJSON $ mkMediumPost path frontmatter post
--        ^ ..create the JSON body for the Medium endpoint and..
      let opts2 =
            defaults
              & Network.Wreq.auth ?~ Network.Wreq.oauth2Bearer (encodeUtf8 apiKey)
              & Network.Wreq.header "Content-Type" .~ [encodeUtf8 "application/json; charset=utf-8"]
      r2 <-
        Network.Wreq.postWith
--      ^ ..post the request to Medium to create the blog post.
          opts
          ("https://api.medium.com/v1/users/" <> Data.Text.unpack id_ <> "/posts")
          json
      print $ r2 ^? responseBody
    e ->
      error $ show e
--    ^ ..else stop execution and display the error `e`.
```

To perform HTTP request we use [Wreq](https://hackage.haskell.org/package/wreq) which employs optics (e.g. `.~`, `^.`). Of course, any other HTTP package would have been ok. Just wanted to have some fun.

The data sent to the Medium endpoint is represented by the `MediumPost` type:

```hs
-- `MediumPost` is what we send to Medium.
data MediumPost = MediumPost
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

-- `Front` is what we parse from the local blog post.
data Front = Front
  { title :: Text,
    description :: Text,
    tags :: [Text]
  }
  deriving (Show, Generic, FromJSON)


mkMediumPost :: Text -> Front -> Text -> MediumPost
mkMediumPost path Front {..} post = MediumPost {..}
--                      ^ Same as `{ title = title, description = description, tags = tags }`.
--                        Enabled by {-# LANGUAGE RecordWildCards #-}.
  where
    publishStatus = "draft"
    content = fold ["Originally posted on", " ", "[odone.io](", canonicalUrl, ").\n\n---\n\n", post]
    contentFormat = "markdown"
    canonicalUrl = urlFor path
    notifyFollowers = True

-- URL of the blog post on odone.io.
urlFor :: Text -> Text
urlFor path = fold [base, "/", name, ".html"]
  where
    base = "https://odone.io/posts"
    name = Data.Text.pack . System.FilePath.Posix.takeBaseName . Data.Text.unpack $ path
```

We then use [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) to get the inputs needed from the command line. Its readme is awesome, so please refer to that to learn more.

```hs
main :: IO ()
main = uncurry crosspost =<< execParser opts
--                           ^ Parses the command line input and returns a tuple (String, String).
--     ^ `uncurry` converts a function on two arguments to a function expecting a tuple.
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
--    ^ The first mandatory argument is the API key to Medium.
      ( metavar "API_KEY"
          <> help "API_KEY to post on Medium"
      )
    <*> Options.Applicative.argument
      str
--    ^ The second mandatory argument is the path to the blog post to crosspost.
      ( metavar "POST"
          <> help "Path to blog POST to crosspost"
      )
```

With that in place, calling the script without the mandatory arguments we get:

```bash
./tomedium.hs
# Missing: API_KEY POST
#
# Usage: tomedium.hs API_KEY POST
#   Crossposts POST to Medium
```

We can also call it with `--help` to get a detailed explanation:

```bash
./tomedium.hs --help
# Usage: tomedium.hs API_KEY POST
#   Crossposts POST to Medium
#
# Available options:
#   API_KEY                  API_KEY to post on Medium
#   POST                     Path to blog POST to crosspost
#   -h,--help                Show this help text
```

A proper call adds an unpublished blog post on Medium with all the following filled properly:

- title;
- description;
- tags;
- canonical\_url (the URL of the post on [odone.io](https://odone.io));
- content.

The whole script is on [GitHub](https://github.com/3v0k4/contact-page/blob/6863fcd7c5a570269f93e94cb50a84f7e67161be/blog/tomedium.hs).

The fact that the script is based on the one for DevTo and written in Haskell made my life really easy. Yet another great reason to write scripts using static strong types.
