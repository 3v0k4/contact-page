---
title: Mars Rover Kata in Haskell
description: Moving a rover on Mars type-safely
author: Riccardo
cover_image: https://odone.io/images/mars-rover.jpg
series: Mars Rover Kata in Haskell
tags:
  - Functional Programming
  - Haskell
---

Today we will TDD the [Mars Rover Kata](https://archive.codeplex.com/?p=marsroverexercise) with the T in TDD meaning Type!

## Red

The first step is to define all the types we need for our program to do it's job:

```hs
type Coord =
    (Int, Int)

type CoordDir =
    (Coord, Dir)

data Turn
    = L
    | R

data Dir
    = N
    | E
    | S
    | W

data Cmd
    = Turn Turn
    | Move

readMaybeT :: Read a => Text -> Maybe a

parsePlateauMax :: Text -> Maybe Coord

parseStartingCoordDir :: Text -> Maybe CoordDir

parseCommands :: Text -> Maybe [Cmd]

runCommands :: [Cmd] -> Coord -> CoordDir -> CoordDir

run :: Text -> Maybe CoordDir
```

Ideally we would use the functions above as follows:

```hs
main :: IO ()
main = do
    print $ parsePlateauMax "5 5"
    -- Just (5,5)

    print $ parseStartingCoordDir "1 2 N"
    -- Just ((1,2),N)

    print $ parseCommands "LMLMLMLMM"
    -- Just [Turn L,Move,Turn L,Move,Turn L,Move,Turn L,Move,Move]

    let commands = "5 5\n1 2 N\nLMLMLMLMM"
    maybe (print "ERR!") print $ run commands
    -- ((1,3),N)
```

## Green

With the types in place it's enough to make it compile :D

```hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T (lines, words, unpack)
import Text.Read (readMaybe)

type Coord =
    (Int, Int)

type CoordDir =
    (Coord, Dir)

data Turn
    = L
    | R
    deriving (Show, Read)

data Dir
    = N
    | E
    | S
    | W
    deriving (Show, Read)

data Cmd
    = Turn Turn
    | Move
    deriving (Show)

readMaybeT :: Read a => Text -> Maybe a
readMaybeT = readMaybe . T.unpack

parsePlateauMax :: Text -> Maybe Coord
parsePlateauMax txt =
    case T.words txt of
        [x, y] -> (,) <$> readMaybeT x <*> readMaybeT y
        _      -> Nothing

parseStartingCoordDir :: Text -> Maybe CoordDir
parseStartingCoordDir txt = do
    (x, y, d) <- case T.words txt of
        [x, y, d] -> (,,) <$> readMaybeT x <*> readMaybeT y <*> readMaybeT d
        _      -> Nothing
    Just ((x, y), d)

parseCommands :: Text -> Maybe [Cmd]
parseCommands txt = toCmd `traverse` T.unpack txt
    where
        toCmd :: Char -> Maybe Cmd
        toCmd 'L' = Just (Turn L)
        toCmd 'R' = Just (Turn R)
        toCmd 'M' = Just Move
        toCmd _   = Nothing


runCommands :: [Cmd] -> Coord -> CoordDir -> CoordDir
runCommands []       _   coordDir             = coordDir
runCommands (c:cmds) max coordDir = runCommands cmds max $ newCoordDir c
    where
        newCoordDir (Turn L) = turnL coordDir
        newCoordDir (Turn R) = turnR coordDir
        newCoordDir Move     = move coordDir

        turnL (c, N) = (c, W)
        turnL (c, E) = (c, N)
        turnL (c, S) = (c, E)
        turnL (c, W) = (c, S)

        turnR (c, N) = (c, E)
        turnR (c, E) = (c, S)
        turnR (c, S) = (c, W)
        turnR (c, W) = (c, N)

        move ((x, y), N) = (wrap max (x, y+1), N)
        move ((x, y), E) = (wrap max (x+1, y), E)
        move ((x, y), S) = (wrap max (x, y-1), S)
        move ((x, y), W) = (wrap max (x-1, y), W)

        wrap (maxX, maxY) (x, y) = (wrap' maxX x, wrap' maxY y)

        wrap' max x | x < 0       = max + 1 + x
        wrap' max x | x > max     = x `rem` max
        wrap' max x | otherwise   = x


run :: Text -> Maybe CoordDir
run txt = do
    (max, coordDir, cmds) <- go $ T.lines txt
    Just $ runCommands cmds max coordDir
    where
        go [f, s, t] = (,,) <$> parsePlateauMax f <*> parseStartingCoordDir s <*> parseCommands t
        go _         = Nothing

main :: IO ()
main = do
    let commands = "5 5\n1 2 N\nLMLMLMLMM"
    print $ parsePlateauMax "5 5"
    print $ parseStartingCoordDir "1 2 N"
    print $ parseCommands "LMLMLMLMM"
    maybe (print "ERR!") print $ run commands
```

## Refactor

I don't have any more time for today. We will be refactoring in a future post!
