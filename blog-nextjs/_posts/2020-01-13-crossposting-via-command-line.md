---
title: Crossposting to DevTo via command line
description: Using a Haskell script to crosspost a blog post with a frontmatter as used in Jekyll markdown files
author: Riccardo
tags:
  - Functional Programming
  - Haskell
  - Script
---

Another week, another script in Haskell. It's probably clear that I've found a safe heaven where to play with functional concepts. I do the same at work. Do we need to automate something simple? Maybe just run once and forget it forever? No need to talk more, my fingers are already typing `vim script.hs`.

Two weeks ago we covered a [script to scaffold a blog post](https://odone.io/posts/2019-12-26-scaffolding-a-blog-post.html). Last week we talked about [tweeting a blog post from the command line](https://odone.io/posts/2020-01-06-posting-a-tweet-with-haskell.html). Today it's time to crosspost a local Jekyll-like blog post to [dev.to](https://dev.to).

Let's start from the core of the script, the `crosspost` function:

```hs
crosspost :: String -> String -> IO ()
crosspost apiKey path = do
--        ^ DevTo API key.
--               ^ Filepath to the blog post to crosspost.
  f <- Data.ByteString.readFile path
  case parseYamlFrontmatter f of
--     ^ Parse the frontmatter (and content) of the blog post to crosspost.
      Done postBS frontmatter -> do
--    ^ If the frontmatter (and content) was parsed successfully..
        let post = Data.ByteString.Char8.unpack postBS
        let json = toJSON $ mkDevPost path frontmatter post
--      ^ ..then create the JSON body for the DevTo endpoint and..
        let opts = defaults & Network.Wreq.header "api-key" .~ [Data.ByteString.Char8.pack apiKey]
                            & Network.Wreq.header "Content-Type" .~ [Data.ByteString.Char8.pack "application/json; charset=utf-8"]
        r <- Network.Wreq.postWith opts "https://dev.to/api/articles" json
--           ^ ..post the request to DevTo to create the blog post.
        print $ r ^. responseStatus
      e ->
        error $ show e
--      ^ ..else stop execution and display the error `e`.
```

Notice that the script uses [wreq](https://hackage.haskell.org/package/wreq) to perform the HTTP request. The library uses optics heavily (e.g. `.~`, `^.`). We could have used any other library and avoided them easily.

The data sent to the DevTo endpoint is represented by the `DevPost` type:

```hs
-- `Front` is what we parse from the local blog post.
data Front =
  Front
    { title :: String
    , description :: String
    , tags :: [String]
    } deriving (Show, Generic, FromJSON)

-- `DevPost` is what we send to DevTo.
data DevPost =
  DevPost
    { title :: String
    , description :: String
    , tags :: [String]
    , canonical_url :: String
    , published :: Bool
    , body_markdown :: String
    } deriving (Show, Generic)

mkDevPost :: String -> Front -> String -> DevPost
mkDevPost path Front{..} post = DevPost{..}
--                  ^ Same as `{ title = title, description = description, tags = tags }`.
--                    Enabled by {-# LANGUAGE RecordWildCards #-}.
  where
    published = False
    body_markdown = post
    canonical_url = urlFor path

-- URL of the blog post on odone.io.
urlFor :: String -> String
urlFor path = fold [base, "/", name, ".html"]
  where
    base = "https://odone.io/posts"
    name = System.FilePath.Posix.takeBaseName path
```

We then use [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) to get the inputs needed from the command line. Its readme is awesome, so please refer to that to learn more.

```hs
main :: IO ()
main = uncurry crosspost =<< execParser opts
--                           ^ Parses the command line input and returns a tuple (String, String).
--     ^ `uncurry` converts a function on two arguments to a function expecting a tuple.
  where
    opts = info (parser <**> helper)
      (  fullDesc
      <> progDesc "Crossposts POST to DevTo"
      )

parser :: Options.Applicative.Parser (String, String)
parser = (,)
    <$>
      Options.Applicative.argument str
--    ^ The first mandatory argument is the API key to DevTo.
        (  metavar "API_KEY"
        <> help "API_KEY to post on DevTo"
        )
    <*>
      Options.Applicative.argument str
--    ^ The second mandatory argument is the path to the blog post to crosspost.
        (  metavar "POST"
        <> help "Path to blog POST to post on DevTo"
        )
```

With that in place, calling the script without the mandatory arguments we get:

```bash
$ ./todevto.hs
#
# Missing: API_KEY POST
#
# Usage: todevto.hs API_KEY POST
#   Crossposts POST to DevTo
```

We can also call it with `--help` to get a detailed explanation:

```bash
$ ./todevto.hs --help
#
# Usage: todevto.hs API_KEY POST
#   Crossposts POST to DevTo
#
# Available options:
#   API_KEY                  API_KEY to post on DevTo
#   POST                     Path to blog POST to post on DevTo
#   -h,--help                Show this help text
```

A proper call adds an unpublished blog post on DevTo with all the following filled properly:

- title;
- description;
- tags;
- canonical_url (the URL of the post on [odone.io](https://odone.io));
- content.

The whole script is on [GitHub](https://github.com/3v0k4/contact-page/blob/ba7b8ceab98f1cd19765dc94f5adee6a446719ae/blog/todevto.hs).
