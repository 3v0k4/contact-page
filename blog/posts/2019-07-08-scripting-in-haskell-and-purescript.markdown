---
title: Scripting in Haskell and PureScript
author: Riccardo
description: Writing scripts with Haskell and PureScript is a good way of doing some functional programming at work
cover_image: https://odone.io/images/scripting.jpg
---

When learning how to jump with the snowboard, my trainer used to say: "you need to take a lot of air". In other words, if you want to learn how to jump, you need to jump a lot.

Functional programming is a skill like many others. Thus, we need to do a lot of it to get fluent with the paradigm. This is difficult since most of us at work do object oriented programming.

Recently, at [Monadic Party](http://monadic.party) somebody told me that a good trick to do functional at work is to write scripts. I would have never considered Haskell or PureScript as scripting languages. Turns out it's not only easy to do, but also a great idea to do more FP.

In this post, we are gonna see the setup needed to write scripts in both Haskell and PureScript. Also, we are gonna take a look at the Haskell script I wrote at Monadic Party to test the idea.

## Haskell Stack Scripting

As the website puts it

> Stack is a cross-platform program for developing Haskell projects

The best thing is that Stack provides an [interpreter](https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter) for scripts.

The "Hello, World!" would be

```hs
#!/usr/bin/env stack
{- stack
   script
   --resolver lts-13.27
-}

main :: IO ()
main = putStrLn "Hello, World!"
```

That can be run with

```bash
chmod +x MY_SCRIPT
./MY_SCRIPT

# Hello, World!
```

It's possible to import packages as any other Haskell program. For example, the following script uses [`directory`](http://hackage.haskell.org/package/directory-1.3.4.0) to do something similar to `ls` in Bash:

```hs
#!/usr/bin/env stack
{- stack
   script
   --resolver lts-13.27
   --package directory
-}

import Data.List (intercalate)
import System.Directory (listDirectory)

main :: IO ()
main = do
  entries <- listDirectory "."
  putStrLn $ intercalate " " entries
```

## PureScript Scripting

Since PureScript compiles to JavaScript we can use Node as our interpreter. First, we scaffold a PureScript project with `pulp init`

```bash
mkdir MY_SCRIPT_DIR && cd MY_SCRIPT_DIR
pulp init
```

That generates a `src/Main.purs` that looks like

```hs
module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "Hello sailor!"
```

Then we can compile to a Node file with

```bash
pulp build --to output/index.js
```

And interpret the file with `node`

```bash
node output/index.js
# Hello sailor!
```

Alternatively we can create a small shim and interpret it with the `node` interpreter

```bash
echo '#!/usr/bin/env node\n\nrequire("./output/index.js");' > MY_SCRIPT
chmod +x MY_SCRIPT
./MY_SCRIPT
# Hello sailor!
```

The `ls` script in PureScript uses two additional packages that we can install with

```bash
bower install --save purescript-node-fs
bower install --save purescript-strings
```

and looks like

```hs
module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readdir)
import Data.String.Common (joinWith)

main :: Effect Unit
main = do
  entries <- readdir "."
  log $ joinWith " " entries
```

The downside of scripting with PureScript is that we need to keep around the entire "project" directory. With Stack there's only the script file to take care of.

## A non-trivial Haskell Stack Script

```hs
#!/usr/bin/env stack
{- stack
  script
  --resolver nightly-2019-06-20
  --package directory
  --package req
  --package text
  --package aeson
  --package process
  --package parsec
  --package filepath
  --package unix
-}

-- This script creates a Brewfile using `brew bundle dump`
-- and adds to that all the apps installed in `/Applications`
-- that can be installed via HomeBrew as casks.
-- Later you can use `brew bundle` to install or upgrade
-- all dependencies listed in the Brewfile.
-- It can be useful to restore the same packages and apps
-- on a different Mac.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

import System.Directory
import Data.Char
import Network.HTTP.Req
import qualified Data.Text as T
import Control.Monad.IO.Class
import GHC.Generics
import Data.Aeson
import Data.List
import System.Process
import Text.Parsec
import System.FilePath.Posix
import System.Posix.Files
import System.Exit
import Control.Monad

data Package =
  Package { name :: [String] } deriving (Generic, Show)

data BrewfileLine
  = Tap String
  | Brew String
  | Cask String
  deriving (Eq)

instance Show BrewfileLine where
  show (Tap s) = "tap \"" <> s <> "\""
  show (Brew s) = "brew \"" <> s <> "\""
  show (Cask s) = "cask \"" <> s <> "\""

instance Ord BrewfileLine where
  (<=) (Tap s1) (Tap s2) = fmap toLower s1 <= fmap toLower s2
  (<=) (Tap _) _ = True
  (<=) (Brew s1) (Brew s2) = fmap toLower s1 <= fmap toLower s2
  (<=) (Brew _) _ = True
  (<=) (Cask s1) (Cask s2) = fmap toLower s1 <= fmap toLower s2
  (<=) (Cask _) _ = False

instance ToJSON Package where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Package

data Response =
  Response [Package] deriving (Generic, Show)

instance ToJSON Response where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Response

main :: IO ()
main = do
  doesBrewfileExist <- fileExist "Brewfile"
  when doesBrewfileExist $ die "Brewfile already exists! Aborted."
  installed <- getInstalledApps
  installable <- fetchInstallableAppsWithBrew
  let casks = installed `intersect` installable
  lines <- getBrewDumpLines
  let all = union casks <$> lines
  either
    (die . show)
    (writeBrewfile >=> \_ -> putStrLn "Brewfile generated!")
    all

getInstalledApps :: IO [BrewfileLine]
getInstalledApps = do
  filePaths <- listDirectory "/Applications"
  let names = takeBaseName <$> filePaths
  pure $ Cask <$> names

fetchInstallableAppsWithBrew :: IO [BrewfileLine]
fetchInstallableAppsWithBrew = runReq defaultHttpConfig $ do
  res <- req
    GET
    (https (T.pack "formulae.brew.sh") /: (T.pack "api") /: (T.pack "cask.json"))
    NoReqBody
    jsonResponse
    mempty
  pure $ fmap Cask $ unNames $ (responseBody res :: Response)

unNames :: Response -> [String]
unNames (Response xs) = unName <$> xs

unName :: Package -> String
unName (Package name) = head name

writeBrewfile :: [BrewfileLine] -> IO ()
writeBrewfile lines = do
  let lines' = unlines $ fmap show $ sort $ nub lines
  writeFile "Brewfile" lines'

getBrewDumpLines :: IO (Either ParseError [BrewfileLine])
getBrewDumpLines = do
  out <- readProcess "brew" ["bundle", "dump", "--file=/dev/stdout"] []
  pure $ parse brewfileParser "" out

brewfileParser :: Stream s m Char => ParsecT s u m [BrewfileLine]
brewfileParser = endBy1 brewfileLine $ char '\n'

brewfileLine :: Stream s m Char => ParsecT s u m BrewfileLine
brewfileLine = brewfileLine' "tap" Tap <|> brewfileLine' "brew" Brew <|> brewfileLine' "cask" Cask

brewfileLine' :: Stream s m Char => String -> (String -> BrewfileLine) -> ParsecT s u m BrewfileLine
brewfileLine' prefix constructor = do
  string $ prefix <> " "
  name <- quoted
  pure $ constructor name

quote :: Stream s m Char => ParsecT s u m Char
quote = char '"'

quoted :: Stream s m Char => ParsecT s u m String
quoted = between quote quote (many1 $ noneOf "\"")
```

## Outro

Special thanks to [Justin](https://twitter.com/jusrin00) from whom I blatantly copied half of this post. Be sure to check his ["Write a simple CLI in PureScript"](https://qiita.com/kimagure/items/39b26642b89bd87bf177).
