---
title: Parsing CSV in Haskell
description: Using Cassava to parse a CSV file
author: Riccardo
cover_image: https://odone.io/images/csv.jpg
tags:
  - Functional Programming
  - Haskell
---

Parsing CSV without header:

```hs
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Data.Csv

-- file.csv
-- 1,2
-- 3,4

main :: IO ()
main = do
    f <- BL.readFile "file.csv"
    case decode NoHeader f of
        Left err -> print err
        Right xs -> V.forM_ xs $ \(x :: Int, y :: Int) -> print (x, y)
    -- 1,2
    -- 3,4
```

Parsing CSV to a data type requires having an instance of `FromRecord` for that type:

```hs
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Control.Monad (mzero)

data Coords =
    Coords Int Int

instance FromRecord Coords where
    parseRecord xs
        | length xs == 2 = Coords <$> (xs .! 0) <*> (xs .! 1)
        | otherwise      = mzero

-- file.csv
-- 1,2
-- 3,4

main :: IO ()
main = do
    f <- BL.readFile "file.csv"
    case decode NoHeader f of
        Left err -> print err
        Right xs -> V.forM_ xs $ \(Coords x y) -> print (x, y)
    -- 1,2
    -- 3,4
```

Or the same thing using generics:

```hs
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import GHC.Generics (Generic)

data Coords =
    Coords Int Int
    deriving (Generic)

instance FromRecord Coords
instance ToRecord Coords

-- file.csv
-- 1,2
-- 3,4

main :: IO ()
main = do
    f <- BL.readFile "file.csv"
    case decode NoHeader f of
        Left err -> print err
        Right xs -> V.forM_ xs $ \(Coords x y) -> print (x, y)
    -- 1,2
    -- 3,4
```

Parsing CSV with explicit reference to the fields. This way the code is more robust to changes in the CSV (e.g. change order of columns):

```hs
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Data.Csv

data Coords =
    Coords Int Int

instance FromNamedRecord Coords where
    parseNamedRecord x = Coords <$> (x .: "x") <*> (x .: "y")

-- file.csv
-- x,y
-- 1,2
-- 3,4

main :: IO ()
main = do
    f <- BL.readFile "file.csv"
    case decodeByName f of
        Left err      -> print err
        Right (_, xs) -> V.forM_ xs $ \(Coords x y) -> print (x, y)
    -- 1,2
    -- 3,4
```

Or the same thing using generics:

```hs
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import GHC.Generics (Generic)

data Coords =
    Coords { x :: Int, y :: Int }
    deriving (Generic)

instance FromNamedRecord Coords
instance ToNamedRecord Coords
instance DefaultOrdered Coords

-- file.csv
-- x,y
-- 1,2
-- 3,4

main :: IO ()
main = do
    f <- BL.readFile "file.csv"
    case decodeByName f of
        Left err      -> print err
        Right (_, xs) -> V.forM_ xs $ \(Coords x y) -> print (x, y)
    -- 1,2
    -- 3,4
```
