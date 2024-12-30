---
title: A Parser Combinator in PureScript (part 1/2)
author: Riccardo
description: Implementing and using a parser combinator
cover_image: /images/parser.jpeg
series: A Parser Combinator in PureScript
tags:
  - Functional Programming
  - PureScript
---

Let's start from defining the `Parser` type. The idea is that a parser is a function that takes a string and tries to "parse" something out of it. 

Whenever parsing fails the function returns `Nothing`. Otherwise, a tuple of results is produced: an intermediate value (the parsed value) and the remainder of the string.

This is really similar to the intuition behind the State Monad. The [previous post](https://dev.to/riccardoodone/a-state-monad-in-purescript-p1k) is all about it!

The `Parser` type is defined as follows:

```hs
newtype Parser a = Parser (String -> Maybe (Tuple a String))
```

Then we need a way to run a parser given a string to parse:

```hs
runParser :: forall a. Parser a -> String -> Maybe (Tuple a String)
runParser (Parser g) s = g s
```

To allow composition of parsers we define instances up to Monad:

```hs
instance functorParser :: Functor Parser where
    -- map :: forall a b. (a -> b) -> f a -> f b
    map g f = Parser (\s -> case runParser f s of
                              Just (Tuple v s') -> Just $ Tuple (g v) s'
                              Nothing           -> Nothing)

instance applyParser :: (Functor Parser) => Apply Parser where
    -- apply :: forall a b. f (a -> b) -> f a -> f b
    apply fg f = Parser (\s -> case runParser fg s of
                                 Just (Tuple g s') ->
                                   case runParser f s' of
                                     Just (Tuple v s'') -> Just $ Tuple (g v) s''
                                     Nothing            -> Nothing
                                 Nothing           -> Nothing)

instance applicativeParser :: (Apply Parser) => Applicative Parser where
    -- pure :: forall a. a -> f a
    pure x = Parser (\s -> Just $ Tuple x s)

instance bindParser :: (Apply Parser) => Bind Parser where
    -- bind :: forall a b. m a -> (a -> m b) -> m b
    bind m g = Parser (\s -> case runParser m s of
                               Just (Tuple v s') -> runParser (g v) s'
                               Nothing           -> Nothing)

instance monadParser :: (Bind Parser) => Monad Parser
```

## Implementing Parsers

The simplest parser we can create is the one that always fails:

```hs
fail :: forall a. Parser a
fail = Parser (\_ -> Nothing)
```

The second one is a parser that consumes a `Char`:

```hs
anyChar :: Parser Char
anyChar = Parser (\s -> case toChars s :: List Char of
                          Nil       -> Nothing
                          Cons x xs -> Just $ Tuple x $ fromChars xs)
```

In other words, in case the string is empty it fails, otherwise it produces a tuple of an intermediate result (i.e. the first character of the string) and the remainder of the string.

Example:

```hs
main :: Effect Unit
main = do
  logShow $ runParser anyChar "string"
  -- (Just (Tuple 's' "tring"))
```

The third parser is the one that consumes a `Char` that satisfies a predicate. This is where we start using the Monad instance to both have access to the intermediate value and be able to fail:

```hs
sat :: (Char -> Boolean) -> Parser Char
sat pred = do
    c <- anyChar
    if pred c then pure c else fail
```

With `sat` in our toolbox we can introduce many more parsers:

```hs
digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat ((==) c)
```

Example

```hs
main :: Effect Unit
main = do
  logShow $ runParser (char 's') "string"
  -- (Just (Tuple 's' "tring"))

  logShow $ runParser (char 'Z') "string"
  -- Nothing

  logShow $ runParser digit "3tring"
  -- (Just (Tuple '3' "tring"))
```


We can also use recursive calls and create more sophisticated parsers. For example a `string` parser:

```hs
string :: String -> Parser String
string s =
  map fromChars $ case toChars s :: List Char of
       Nil       -> pure Nil
       Cons x xs -> do
          _ <- char x
          _ <- string $ fromChars xs
          pure $ Cons x xs
```

This parser just keeps trying to parse the next character from the passed string `s`. The intermediate result (i.e. `_ <- char x`) is always discarded. That's because either

- The parser gets to the end of the string `s` by parsing each character successfully. That means the intermediate value is exactly `s` (i.e. the recursive `Cons x xs`)
- The parser fails parsing any character of the string `s` and close circuits to `Nothing`

Example

```hs
main :: Effect Unit
main = do
  logShow $ runParser (string "stri") "string"
  -- (Just (Tuple "stri" "ng"))

  logShow $ runParser (string "ZZZ") "string"
  -- Nothing
```

## The Whole Code

```hs
module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Data.Maybe
import Data.Tuple
import Data.String.Yarn
import Data.List
import Data.Char.Unicode
import Data.String.CodeUnits as CU

newtype Parser a = Parser (String -> Maybe (Tuple a String))

runParser :: forall a. Parser a -> String -> Maybe (Tuple a String)
runParser (Parser g) s = g s

instance functorParser :: Functor Parser where
    -- map :: forall a b. (a -> b) -> f a -> f b
    map g f = Parser (\s -> case runParser f s of
                              Just (Tuple v s') -> Just $ Tuple (g v) s'
                              Nothing           -> Nothing)

instance applyParser :: (Functor Parser) => Apply Parser where
    -- apply :: forall a b. f (a -> b) -> f a -> f b
    apply fg f = Parser (\s -> case runParser fg s of
                                 Just (Tuple g s') ->
                                   case runParser f s' of
                                     Just (Tuple v s'') -> Just $ Tuple (g v) s''
                                     Nothing            -> Nothing
                                 Nothing           -> Nothing)

instance applicativeParser :: (Apply Parser) => Applicative Parser where
    -- pure :: forall a. a -> f a
    pure x = Parser (\s -> Just $ Tuple x s)

instance bindParser :: (Apply Parser) => Bind Parser where
    -- bind :: forall a b. m a -> (a -> m b) -> m b
    bind m g = Parser (\s -> case runParser m s of
                               Just (Tuple v s') -> runParser (g v) s'
                               Nothing           -> Nothing)

instance monadParser :: (Bind Parser) => Monad Parser

fail :: forall a. Parser a
fail = Parser (\_ -> Nothing)

anyChar :: Parser Char
anyChar = Parser (\s -> case toChars s :: List Char of
                          Nil       -> Nothing
                          Cons x xs -> Just $ Tuple x $ fromChars xs)

sat :: (Char -> Boolean) -> Parser Char
sat pred = do
    c <- anyChar
    if pred c then pure c else fail

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat ((==) c)

string :: String -> Parser String
string s =
  map fromChars $ case toChars s :: List Char of
       Nil       -> pure Nil
       Cons x xs -> do
          _ <- char x
          _ <- string $ fromChars xs
          pure $ Cons x xs

main :: Effect Unit
main = do
  logShow $ runParser anyChar "string"
  -- (Just (Tuple 's' "tring"))

  logShow $ runParser (char 's') "string"
  -- (Just (Tuple 's' "tring"))

  logShow $ runParser (char 'Z') "string"
  -- Nothing

  logShow $ runParser digit "3tring"
  -- (Just (Tuple '3' "tring"))

  logShow $ runParser (string "stri") "string"
  -- (Just (Tuple "stri" "ng"))

  logShow $ runParser (string "ZZZ") "string"
  -- Nothing
```
