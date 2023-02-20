---
title: A Parser Combinator in PureScript (part 2/2)
author: Riccardo
description: Implementing and using a parser combinator
cover_image: https://odone.io/images/parser-2.png
series: A Parser Combinator in PureScript
tags:
  - Functional Programming
  - PureScript
---

In the [previous post](https://odone.io/posts/2019-06-24-a-parser-combinator-in-purescript-part-1/) we've created the basic building blocks of a parser combinator library. Now it's time to add some more fancy stuff.

## Choice and Failure

Up until now, each parser either parsed successfully or failed. We'd like to have a parser that can try different parsers before giving up and failing.

Luckily, math already did the dirty job for us. In fact, we can use a couple of typeclasses: [`Alt`](https://pursuit.purescript.org/packages/purescript-control/4.1.0/docs/Control.Alt#t:Alt) and [`Plus`](https://pursuit.purescript.org/packages/purescript-control/4.1.0/docs/Control.Plus#t:Plus).

`Alt` captures the intuition of a choice between alternatives. It has only one member `alt` or infix `<|>`:

```hs
instance altParser :: (Functor Parser) => Alt Parser where
    -- alt :: forall a. f a -> f a -> f a
    alt p1 p2 = Parser (\s -> case runParser p1 s of
                                Just (Tuple v s') -> Just $ Tuple v s'
                                Nothing           -> runParser p2 s)
```

In other words, it tries `p1` and falls back to `p2` in case of failure. For example

```hs
main :: Effect Unit
main = do
  logShow $ runParser (char 'Z' <|> char 's') "string"
  -- (Just (Tuple 's' "tring"))
```

`Plus`represents failure:

```hs
instance plusParser :: (Alt Parser) => Plus Parser where
    -- empty :: forall a. f a
    empty = Parser (\_ -> Nothing)
```

In the previous post, we represented failure in the `fail` parser. Turns out we could have used `empty` in its place:

```hs
main :: Effect Unit
main = do
  -- `:: Parser Unit` is needed to make the compiler happy
  logShow $ runParser (empty :: Parser Unit) "string"
  -- Nothing
```

Putting it together

```hs
main :: Effect Unit
main = do
  logShow $ runParser (empty <|> anyChar) "string"
  -- (Just (Tuple 's' "tring"))

  logShow $ runParser (anyChar <|> empty) "string"
  -- (Just (Tuple 's' "tring"))

  logShow $ runParser (char 'Z' <|> char 's') "string"
  -- (Just (Tuple 's' "tring"))
```

## Attempt Parsing Multiple Times

It would be awesome to be able to use one parser as many times as it can succeed. Let's say we wanted to match any number of spaces. Then we could write

```hs
spaces :: Parser Unit
spaces = do
  c <- anyChar
  if c == ' '
    then spaces
    else Parser (\s -> Just (Tuple unit $ fromChars [c] <> s))
```

In other words, we parse any character, if it's a space we recursively call `spaces`. Otherwise, we noop by putting back the character in the string.

Or similarly

```hs
spaces' :: Parser Unit
spaces' = (char ' ' >>= \_ -> spaces') <|> pure unit
```

we either succeed at parsing a space and recursively call `spaces'` or we get out successfully.

Those work

```hs
main :: Effect Unit
main = do
  logShow $ runParser spaces "   string"
  -- (Just (Tuple Unit "string"))

  logShow $ runParser spaces "string"
  -- (Just (Tuple Unit "string"))

  logShow $ runParser spaces' "   string"
  -- (Just (Tuple Unit "string"))

  logShow $ runParser spaces' "string"
  -- (Just (Tuple Unit "string"))
```

However, they look ugly. But guess what? Math has our back once again. As a matter of fact, we can use [`Data.List.many`](https://pursuit.purescript.org/packages/purescript-lists/5.4.0/docs/Data.List.Lazy#v:many).

To use `many`, we need to satisfy a couple of constraints. In fact, the signature is

```hs
many
  :: forall f a. Alternative f
  => Lazy (f (List a))
  => f a
  -> f (List a)
```

If we substitute `Parser` in place of `f` then we get

```hs
many
  :: forall a. Alternative Parser
  => Lazy (Parser (List a))
  => Parser a
  -> Parser (List a)
```

That means we need the following instances: `Alternative Parser` and `Lazy (Parser (List a))`.

[`Alternative`](https://pursuit.purescript.org/packages/purescript-control/4.1.0/docs/Control.Alternative#t:Alternative) is easy pease because it has no members to implement. In particular, for a type to be an `Alternative`, it only needs to be both `Applicative` and `Plus`. Since `Parser` implements those, we can just write

```hs
instance alternativeParser :: (Applicative Parser, Plus Parser) => Alternative Parser
```

Attempting to parse an indefinite amount of times requires laziness for the same reason we need laziness to have an infinite list (check the mutual recursive definition of [`many`](https://github.com/purescript/purescript-lists/blob/6629e0c05f2ee4b47a9b2fbcdbe6619ff17e8e28/src/Data/List.purs#L167-L167) and [`some`](https://github.com/purescript/purescript-lists/blob/6629e0c05f2ee4b47a9b2fbcdbe6619ff17e8e28/src/Data/List.purs#L155) to dig deeper).

In Haskell that is not a problem because the language is lazy by default. In PureScript we need to use [`Lazy`](https://pursuit.purescript.org/packages/purescript-control/4.1.0/docs/Control.Lazy#t:Lazy):

```hs
instance lazyParser :: Lazy (Parser (List a)) where
    -- defer :: (Unit -> l) -> l
    defer g = Parser (\s -> runParser (g unit) s)
```

That means now we can write

```hs
spaces0 :: Parser Unit
spaces0 = do
  _ <- many $ char ' '
  pure unit
```

We discard the spaces (`_ <- many $ char ' '`) because we are not interested in a list of spaces, we just want to consume them:

```hs
main :: Effect Unit
main = do
  logShow $ runParser spaces0 "   string"
  -- (Just (Tuple Unit "string"))

  logShow $ runParser spaces0 "string"
  -- (Just (Tuple Unit "string"))
```

The best part is that now we can also use `some` to have a parser that needs at least one match to succeed:

```hs
spaces1 :: Parser Unit
spaces1 = do
  _ <- some $ char ' '
  pure unit

main :: Effect Unit
main = do
  logShow $ runParser spaces0 "   string"
  -- (Just (Tuple Unit "string"))

  logShow $ runParser spaces0 "string"
  -- Nothing
```

## The `int` Parser

Just for fun, let's put everything together into a parser that parses `Int`s.

At the top we define `int` as

```hs
int :: Parser Int
int =
  neg <|> nat
```

In other words, an int is either a negative or a natural. `nat` is implemented as follows

```hs
nat :: Parser Int
nat = do
  xs <- some digit
  case fromString $ fromChars xs of
    Just n -> pure n
    Nothing -> empty 
```

And `neg` as follows

```hs
neg :: Parser Int
neg = do
  _ <- char '-'
  negate <$> nat
```

Et voilà

```hs
main :: Effect Unit
main = do
  logShow $ runParser int "123 string"
  -- (Just (Tuple 123 " string"))

  logShow $ runParser int "-123 string"
  -- (Just (Tuple -123 " string"))

  logShow $ runParser int "string"
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
import Data.String.Yarn hiding (fromString)
import Data.List
import Data.Char.Unicode
import Data.String.CodeUnits
import Control.Alt
import Control.Plus
import Control.Alternative
import Control.Lazy (class Lazy)
import Data.Int

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

instance altParser :: (Functor Parser) => Alt Parser where
    -- alt :: forall a. f a -> f a -> f a
    alt p1 p2 = Parser (\s -> case runParser p1 s of
                                Just (Tuple v s') -> Just $ Tuple v s'
                                Nothing           -> runParser p2 s)

instance plusParser :: (Alt Parser) => Plus Parser where
    -- empty :: forall a. f a
    empty = Parser (\_ -> Nothing)

instance alternativeParser :: (Applicative Parser, Plus Parser) => Alternative Parser

instance lazyParser :: Lazy (Parser (List a)) where
    -- defer :: (Unit -> l) -> l
    defer g = Parser (\s -> runParser (g unit) s)

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

spaces :: Parser Unit
spaces = do
  c <- anyChar
  if c == ' '
    then spaces
    else Parser (\s -> Just (Tuple unit $ fromChars [c] <> s))

spaces' :: Parser Unit
spaces' = (char ' ' >>= \_ -> spaces') <|> pure unit

spaces0 :: Parser Unit
spaces0 = do
  _ <- many $ char ' '
  pure unit

spaces1 :: Parser Unit
spaces1 = do
  _ <- some $ char ' '
  pure unit

int :: Parser Int
int =
  neg <|> nat

nat :: Parser Int
nat = do
  xs <- some digit
  case fromString $ fromChars xs of
    Just n -> pure n
    Nothing -> empty

neg :: Parser Int
neg = do
  _ <- char '-'
  negate <$> nat

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

  logShow $ runParser (char 'Z' <|> char 's') "string"
  -- (Just (Tuple 's' "tring"))

  logShow $ runParser (empty :: Parser Unit) "string"
  -- Nothing

  logShow $ runParser (empty <|> anyChar) "string"
  -- (Just (Tuple 's' "tring"))

  logShow $ runParser (anyChar <|> empty) "string"
  -- (Just (Tuple 's' "tring"))

  logShow $ runParser spaces "   string"
  -- (Just (Tuple Unit "string"))

  logShow $ runParser spaces "string"
  -- (Just (Tuple Unit "string"))

  logShow $ runParser spaces' "   string"
  -- (Just (Tuple Unit "string"))

  logShow $ runParser spaces' "string"
  -- (Just (Tuple Unit "string"))

  logShow $ runParser spaces0 "   string"
  -- (Just (Tuple Unit "string"))

  logShow $ runParser spaces0 "string"
  -- (Just (Tuple Unit "string"))

  logShow $ runParser spaces1 "   string"
  -- (Just (Tuple Unit "string"))

  logShow $ runParser spaces1 "string"
  -- Nothing

  logShow $ runParser int "123 string"
  -- (Just (Tuple 123 " string"))

  logShow $ runParser int "-123 string"
  -- (Just (Tuple -123 " string"))

  logShow $ runParser int "string"
  -- Nothing

```

## Outro

Special thanks to Tom for writing ["Fantas, Eel, and Specification 10: Alt, Plus, and Alternative"](http://www.tomharding.me/2017/04/24/fantas-eel-and-specification-10/) and to [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia). Those two provide clear explanations of the intuitions behind `Alternative`. Also, thanks a bunch to @monoidmusician and @kadblas from [fpchat](https://fpchat-invite.herokuapp.com/) who helped me understand `Lazy`.
