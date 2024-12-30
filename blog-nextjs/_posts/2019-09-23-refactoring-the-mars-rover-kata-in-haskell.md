---
title: Refactoring the Mars Rover Kata in Haskell
description: Refactoring our deployed-to-Mars rover
author: Riccardo
cover_image: /images/refactoring-mars-rover.jpg
series: Mars Rover Kata in Haskell
tags:
  - Functional Programming
  - Haskell
---

Last week I received some friendly criticism cause I've ended the [post](/posts/2019-09-16-mars-rover-kata-in-haskell/) by postponing the refactor step of our Type-Driven-Developed kata:

> I don't have any more time for today. We will be refactoring in a future post!

Better make it right ASAP! Let's refactor (for real).

But first, this is the code we got to last week:

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

## CyclicEnum

A couple of weeks ago, we [introduced (i.e. blatantly copied)](/posts/2019-09-09-fun-with-typeclasses/) the `CyclicEnum` typeclass.

Let's use it in our `turnL` and `turnR` functions:

```diff
 data Dir
     = N
     | E
     | S
     | W
-    deriving (Show, Read)
+    deriving (Show, Read, Eq, Enum, Bounded, CyclicEnum)

...

-        turnL (c, N) = (c, W)
-        turnL (c, E) = (c, N)
-        turnL (c, S) = (c, E)
-        turnL (c, W) = (c, S)
+        turnL (c, d) = (c, csucc d)

-        turnR (c, N) = (c, E)
-        turnR (c, E) = (c, S)
-        turnR (c, S) = (c, W)
-        turnR (c, W) = (c, N)
+        turnR (c, d) = (c, cpred d)
```

The code is more tidy but what matters most is that in case we added more `Dir`s like `NE` (i.e. north-east), we wouldn't have to update `turnL` and `turnR`.

Since tuples are functors, we can refactor further to:

```diff
-        turnL (c, d) = (c, csucc d)
+        turnL = fmap csucc

-        turnR (c, d) = (c, cpred d)
+        turnR = fmap cpred
```

and inline `turnL` and `turnR`:

```diff
-        newCoordDir (Turn L) = turnL coordDir
-        newCoordDir (Turn R) = turnR coordDir
+        newCoordDir (Turn L) = fmap csucc coordDir
+        newCoordDir (Turn R) = fmap cpred coordDir
         newCoordDir Move     = move coordDir

-        turnL = fmap csucc
-
-        turnR = fmap cpred
```

## Parsing

To make parsing more solid, we can make use of [Parsec](http://hackage.haskell.org/package/parsec).

We first remove the old code:

```diff
-readMaybeT :: Read a => Text -> Maybe a
-readMaybeT = readMaybe . T.unpack
-
-parsePlateauMax :: Text -> Maybe Coord
-parsePlateauMax txt =
-    case T.words txt of
-        [x, y] -> (,) <$> readMaybeT x <*> readMaybeT y
-        _      -> Nothing
-
-parseStartingCoordDir :: Text -> Maybe CoordDir
-parseStartingCoordDir txt = do
-    (x, y, d) <- case T.words txt of
-        [x, y, d] -> (,,) <$> readMaybeT x <*> readMaybeT y <*> readMaybeT d
-        _      -> Nothing
-    Just ((x, y), d)
-
-parseCommands :: Text -> Maybe [Cmd]
-parseCommands txt = toCmd `traverse` T.unpack txt
-    where
-        toCmd :: Char -> Maybe Cmd
-        toCmd 'L' = Just (Turn L)
-        toCmd 'R' = Just (Turn R)
-        toCmd 'M' = Just Move
-        toCmd _   = Nothing
```

Then, we add the Parsec parsers:

```diff
+toCmd :: Char -> Maybe Cmd
+toCmd 'L' = Just (Turn L)
+toCmd 'R' = Just (Turn R)
+toCmd 'M' = Just Move
+toCmd _   = Nothing
+
+toDir :: Char -> Maybe Dir
+toDir c = readMaybe [c]
+
+dirParser = do
+    dir <- toDir <$> letter
+    maybe (parserFail "not a valid dir") pure dir
+
+cmdParser = do
+    cmd <- toCmd <$> letter
+    maybe (parserFail "not a valid cmd") pure cmd
+
+parser = do
+    min <- read <$> many1 digit
+    space
+    max <- read <$> many1 digit
+    _ <- newline
+    x <- read <$> many1 digit
+    space
+    y <- read <$> many1 digit
+    space
+    d <- dirParser
+    _ <- newline
+    cs <- many1 cmdParser
+    pure ((min, max), ((x, y), d), cs)
```

Finally, we plug the new parser in:

```diff
+eitherToMaybe :: Either a b -> Maybe b
+eitherToMaybe (Right x) = Just x
+eitherToMaybe (Left _)  = Nothing

 run :: Text -> Maybe CoordDir
 run txt = do
-    (max, coordDir, cmds) <- go $ T.lines txt
+    (max, coordDir, cmds) <- eitherToMaybe $ parse parser "" txt
     Just $ runCommands cmds max coordDir
-    where
-        go [f, s, t] = (,,) <$> parsePlateauMax f <*> parseStartingCoordDir s <*> parseCommands t
-        go _         = Nothing
```

Our previous parser returned a `Maybe` but Parsec's `parse` returns `Either`. Thus, we make use of `eitherToMaybe` to plug in the new parser into our old logic.

Since the `Left` case of the `Either` provides some nice information in case parsing fails, let's keep it:

```diff
-eitherToMaybe :: Either a b -> Maybe b
-eitherToMaybe (Right x) = Just x
-eitherToMaybe (Left _)  = Nothing
-
-run :: Text -> Maybe CoordDir
+run :: Text -> Either ParseError CoordDir
 run txt = do
-    (max, coordDir, cmds) <- eitherToMaybe $ parse parser "" txt
-    Just $ runCommands cmds max coordDir
+    (max, coordDir, cmds) <- parse parser "" txt
+    Right $ runCommands cmds max coordDir
```

## BoundedInt

Part of our code is taking care of wrapping the coordinates when they reach the extremes of the grid:

```hs
move ((x, y), N) = (wrap max (x, y+1), N)
move ((x, y), E) = (wrap max (x+1, y), E)
move ((x, y), S) = (wrap max (x, y-1), S)
move ((x, y), W) = (wrap max (x-1, y), W)

wrap (maxX, maxY) (x, y) = (wrap' maxX x, wrap' maxY y)

wrap' max x | x < 0       = max + 1 + x
wrap' max x | x > max     = x `rem` max
wrap' max x | otherwise   = x
```

Let's abstract that logic away.

We first define a `BoundedInt` with an `update` function which takes care of the wrapping:

```diff
+data BoundedInt =
+    BoundedInt Int Int Int
+    deriving (Read, Show)
+
+update :: (Int -> Int) -> BoundedInt -> BoundedInt
+update g (BoundedInt min max val) =
+    BoundedInt min max $ wrap (g val)
+    where
+        wrap x | x < 0     = max + 1 + x `rem` max
+        wrap x | x > max   = x `rem` max
+        wrap x | otherwise = x
```

Then, we update the rest of the code

```diff
 type Coord =
-    (Int, Int)
+    (BoundedInt, BoundedInt)

...

 parser = do
-    min <- read <$> many1 digit
+    maxX <- read <$> many1 digit
     space
-    max <- read <$> many1 digit
+    maxY <- read <$> many1 digit
     _ <- newline
     x <- read <$> many1 digit
     space
     y <- read <$> many1 digit
     space
     d <- dirParser
     _ <- newline
     cs <- many1 cmdParser
-    pure ((min, max), ((x, y), d), cs)
+    pure (((BoundedInt 0 maxX x, BoundedInt 0 maxY y), d), cs)

...

-        move ((x, y), N) = (wrap max (x, y+1), N)
-        move ((x, y), E) = (wrap max (x+1, y), E)
-        move ((x, y), S) = (wrap max (x, y-1), S)
-        move ((x, y), W) = (wrap max (x-1, y), W)
-
-        wrap (maxX, maxY) (x, y) = (wrap' maxX x, wrap' maxY y)
-
-        wrap' max x | x < 0       = max + 1 + x
-        wrap' max x | x > max     = x `rem` max
-        wrap' max x | otherwise   = x
+        move ((x, y), N) = ((x, update (+ 1) y), N)
+        move ((x, y), E) = ((update (subtract 1) x, y), E)
+        move ((x, y), S) = ((x, update (subtract 1) y), S)
+        move ((x, y), W) = ((update (+ 1) x, y), W)
```

## Extracting modules

It should be quite clear by now that our solution relies on specific types: `CoordDir`, `Cmd`, `Turn`. It's time to house them in their own modules.

We start by promoting `CoordDir` to a data type:

```diff
-type CoordDir =
-    (Coord, Dir)
+data CoordDir =
+    CoordDir Coord Dir
+    deriving (Show)

...

 parser = do
     maxX <- read <$> many1 digit
     space
     maxY <- read <$> many1 digit
     _ <- newline
     x <- read <$> many1 digit
     space
     y <- read <$> many1 digit
     space
     d <- dirParser
     _ <- newline
     cs <- many1 cmdParser
-    pure (((BoundedInt 0 maxX x, BoundedInt 0 maxY y), d), cs)
+    pure ((CoordDir (BoundedInt 0 maxX x, BoundedInt 0 maxY y) d), cs)

...

 runCommands :: [Cmd] -> CoordDir -> CoordDir
 runCommands []       coordDir = coordDir
-runCommands (c:cmds) coordDir = runCommands cmds $ newCoordDir c
+runCommands (c:cmds) (CoordDir coord dir) = runCommands cmds $ newCoordDir c
     where
-        newCoordDir (Turn L) = fmap csucc coordDir
-        newCoordDir (Turn R) = fmap cpred coordDir
-        newCoordDir Move     = move coordDir
-
-        move ((x, y), N) = ((x, update (+ 1) y), N)
-        move ((x, y), E) = ((update (subtract 1) x, y), E)
-        move ((x, y), S) = ((x, update (subtract 1) y), S)
-        move ((x, y), W) = ((update (+ 1) x, y), W)
+        newCoordDir (Turn L) = CoordDir coord $ csucc dir
+        newCoordDir (Turn R) = CoordDir coord $ csucc dir
+        newCoordDir Move     = move coord dir
+
+        move (x, y) N = CoordDir (x, update (+ 1) y) N
+        move (x, y) E = CoordDir (update (subtract 1) x, y) E
+        move (x, y) S = CoordDir (x, update (subtract 1) y) S
+        move (x, y) W = CoordDir (update (+ 1) x, y) W
```

Notice how the `move` function either updates the first or second element of the `Coord` tuple. Let's refactor that to use the bifunctor instance of tuple:

```diff
-        move (x, y) N = CoordDir (x, update (+ 1) y) N
-        move (x, y) E = CoordDir (update (subtract 1) x, y) E
-        move (x, y) S = CoordDir (x, update (subtract 1) y) S
-        move (x, y) W = CoordDir (update (+ 1) x, y) W
+        move coord N = CoordDir (second (update (+ 1)) coord) N
+        move coord E = CoordDir (first (update (subtract 1)) coord) E
+        move coord S = CoordDir (second (update (subtract 1)) coord) S
+        move coord W = CoordDir (first (update (+ 1)) coord) W
```

Now we can extract the modules:

```hs
-- Turn.hs

module Turn where

data Turn
    = L
    | R
    deriving (Show, Read)

-- Cmd.hs

module Cmd (Cmd(..), parser) where

import Turn (Turn(..))
import Text.Parsec (parserFail)
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (letter)

data Cmd
    = Turn Turn
    | Move
    deriving (Show)

toCmd :: Char -> Maybe Cmd
toCmd 'L' = Just (Turn L)
toCmd 'R' = Just (Turn R)
toCmd 'M' = Just Move
toCmd _   = Nothing

parser :: Parser Cmd
parser = do
    cmd <- toCmd <$> letter
    maybe (parserFail "not a valid cmd") pure cmd

-- CoordDir.hs

{-# LANGUAGE DeriveAnyClass #-}

module CoordDir (CoordDir, parser, move, turn) where

import Data.Bifunctor (first, second)
import Text.Read (readMaybe)
import Text.Parsec (parserFail)
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (space, digit, newline, letter)
import Text.Parsec.Combinator (many1)
import Turn (Turn(..))

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred d
        | d == minBound = maxBound
        | otherwise = pred d

    csucc :: a -> a
    csucc d
        | d == maxBound = minBound
        | otherwise = succ d

data BoundedInt =
    BoundedInt Int Int Int
    deriving (Read, Show)

type Coord =
    (BoundedInt, BoundedInt)

data Dir
    = N
    | E
    | S
    | W
    deriving (Show, Read, Eq, Enum, Bounded, CyclicEnum)

data CoordDir =
    CoordDir Coord Dir
    deriving (Show)

toDir :: Char -> Maybe Dir
toDir c = readMaybe [c]

dirParser :: Parser Dir
dirParser = do
    dir <- toDir <$> letter
    maybe (parserFail "not a valid dir") pure dir

parser :: Parser CoordDir
parser = do
    maxX <- read <$> many1 digit
    space
    maxY <- read <$> many1 digit
    _ <- newline
    x <- read <$> many1 digit
    space
    y <- read <$> many1 digit
    space
    d <- dirParser
    pure $ CoordDir (BoundedInt 0 maxX x, BoundedInt 0 maxY y) d

update :: (Int -> Int) -> BoundedInt -> BoundedInt
update g (BoundedInt min max val) =
    BoundedInt min max $ wrap (g val)
    where
        wrap x | x < 0     = max + 1 + x `rem` max
        wrap x | x > max   = x `rem` max
        wrap x | otherwise = x

move :: CoordDir -> CoordDir
move (CoordDir coord N) = CoordDir (second (update (+ 1)) coord) N
move (CoordDir coord E) = CoordDir (first (update (subtract 1)) coord) E
move (CoordDir coord S) = CoordDir (second (update (subtract 1)) coord) S
move (CoordDir coord W) = CoordDir (first (update (+ 1)) coord) W

turn :: Turn -> CoordDir -> CoordDir
turn L (CoordDir coord dir) = CoordDir coord $ cpred dir
turn R (CoordDir coord dir) = CoordDir coord $ csucc dir

-- Main.hs

module Main where

import Data.Text (Text)
import Text.Parsec (parse, ParseError)
import Text.Parsec.Char (newline)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Text (Parser)
import Turn (Turn(..))
import CoordDir (CoordDir)
import qualified CoordDir (turn, move, parser)
import Cmd (Cmd(..))
import qualified Cmd (parser)

parser :: Parser (CoordDir, [Cmd])
parser = do
    coordDir <- CoordDir.parser
    _ <- newline
    cmds <- many1 Cmd.parser
    pure (coordDir, cmds)

runCommands :: CoordDir -> [Cmd] -> CoordDir
runCommands = foldl f
    where
        f coordDir (Turn turn) = CoordDir.turn turn coordDir
        f coordDir Move        = CoordDir.move coordDir

run :: Text -> Either ParseError CoordDir
run txt = do
    (coordDir, cmds) <- parse parser "" txt
    pure $ runCommands coordDir cmds

main :: IO ()
main = do
    let commands = "5 5\n1 2 N\nLMLMLMLMM"
    print $ (parse parser "" ("5 5\n1 2 N\nLM" :: Text) :: Either ParseError (CoordDir, [Cmd]))
    either print print $ run commands
```

One important thing to notice is that `CoordDir` internals are not exposed from `CoordDir.hs`. In other words, `CoordDir` is an opaque data type. That means we could refactor the representation of `CoordDir` to a totally different type without breaking other modules.

## Tidying up

We can refactor `runCommands` to replace recursiveness to fold. In fact, we can go from

```hs
runCommands :: [Cmd] -> CoordDir -> CoordDir
runCommands []     coordDir = coordDir
runCommands (x:xs) coordDir = runCommands xs $ newCoordDir x
    where
        newCoordDir (Turn turn) = CoordDir.turn turn coordDir
        newCoordDir Move        = CoordDir.move coordDir
```

to

```hs
runCommands :: CoordDir -> [Cmd] -> CoordDir
runCommands = foldl f
    where
        f coordDir (Turn turn) = CoordDir.turn turn coordDir
        f coordDir Move        = CoordDir.move coordDir
```

## Outro

While refactoring I've experimented with a lot of things. Some ideas miserably failed. For example I've tried to make `BoundedInt` a functor by having a `data Bounded2D a b c = Bounded2D a b c` type. Unfortunately, implementing a sensible `fmap` for that proved to be impossible. I've also tried to use a phantom type to make impossible to update the x coord with a y offset but didn't like how it looked like.

In any case, I'm pretty sure I've missed some cool math abstractions. So please let me know in the comments if you have any ideas!
