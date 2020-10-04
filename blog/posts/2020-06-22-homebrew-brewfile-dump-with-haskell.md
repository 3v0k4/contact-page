---
title: Homebrew Brewfile Dump with Haskell
description: This script creates a Brewfile using `brew bundle dump` and adds to that all the apps from `/Applications` that can be installed via Homebrew as casks.
published: true
author: Riccardo
tags:
  - Functional Programming
  - Haskell
  - Script
---

[Have](https://odone.io/posts/2019-07-08-scripting-in-haskell-and-purescript.html) I [ever](https://odone.io/posts/2019-12-26-scaffolding-a-blog-post.html) mentioned [how](https://odone.io/posts/2020-01-06-posting-a-tweet-with-haskell.html) scripts [are](https://odone.io/posts/2020-01-13-crossposting-via-command-line.html) a [great](https://odone.io/posts/2020-01-20-kanbanery-to-trello.html) way [to](https://odone.io/posts/2020-06-15-crossposting-to-medium-via-command-line.html) put Haskell to use? Here comes another one. In fact, the first Haskell script I have ever wrote. Believe it or not I put it together on the train back from Monadic Party last year.

Today I would write code in a different way. However, the beauty of Haskell is that after several months I can easily make sense of it and refactor without breaking a sweat. It would not be the same had I coded it in Bash.

```hs
#!/usr/bin/env stack
{- stack
  script
  --resolver nightly-2019-06-20
  --package directory
  --package req
  --package aeson
  --package process
  --package parsec
  --package filepath
  --package unix
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- This script creates a Brewfile using `brew bundle dump`
-- and adds to that all the apps from `/Applications`
-- that can be installed via Homebrew as casks.
--
-- Later you can use `brew bundle` to install or upgrade
-- all dependencies listed the Brewfile.
--
-- It can be useful to restore the same packages and apps
-- on a different Mac.

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Char
import Data.List
import GHC.Generics
import Network.HTTP.Req
import System.Directory
import System.Exit
import System.FilePath.Posix
import System.Posix.Files
import System.Process
import Text.Parsec

newtype Response
  = Response [Package]
  deriving (Generic, Show, ToJSON, FromJSON)

newtype Package = Package {name :: [String]}
  deriving (Generic, Show, ToJSON)

instance FromJSON Package where
  parseJSON = withObject "Package" $ \v ->
    Package <$> v .: "name"

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
  res <-
    req
      GET
      (https "formulae.brew.sh" /: "api" /: "cask.json")
      NoReqBody
      jsonResponse
      mempty
  pure . fmap Cask . unNames $ (responseBody res :: Response)

unNames :: Response -> [String]
unNames (Response xs) = unName <$> xs
  where
    unName :: Package -> String
    unName (Package name) = head name

getBrewDumpLines :: IO (Either ParseError [BrewfileLine])
getBrewDumpLines = do
  out <- readProcess "brew" ["bundle", "dump", "--file=/dev/stdout"] []
  pure $ parse brewfileParser "" out

writeBrewfile :: [BrewfileLine] -> IO ()
writeBrewfile =
  writeFile "Brewfile" . unlines . fmap show . sort . nub

-- PARSER

brewfileParser :: Stream s m Char => ParsecT s u m [BrewfileLine]
brewfileParser = endBy1 brewfileLine $ char '\n'

brewfileLine :: Stream s m Char => ParsecT s u m BrewfileLine
brewfileLine =
  brewfileLine' "tap" Tap
    <|> brewfileLine' "brew" Brew
    <|> brewfileLine' "cask" Cask

brewfileLine' :: Stream s m Char => String -> (String -> BrewfileLine) -> ParsecT s u m BrewfileLine
brewfileLine' prefix constructor = do
  string $ prefix <> " "
  name <- quoted
  skipMany $ satisfy (/= '\n')
  pure $ constructor name

quote :: Stream s m Char => ParsecT s u m Char
quote = char '"'

quoted :: Stream s m Char => ParsecT s u m String
quoted = between quote quote (many1 $ noneOf "\"")
```

