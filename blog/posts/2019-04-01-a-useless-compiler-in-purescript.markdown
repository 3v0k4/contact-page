---
title: A Useless Compiler in PureScript
author: Riccardo
---

## Intro

This week I've decided to write some code in PureScript that resembles a compiler. As for the last few posts, I've limited the scope to the smallest possible unit.

That means, I haven't had much time to learn about compilers myself. Hopefully, this will prove useful foundation when I'll decide to dive deeper.

## Let's roll

As long as I understand, a compiler consists of a series of steps that take some input code and transforms it into the target language.

As an input we will use a language that supports two operations (i.e. `add` and `sub`) on integers. In particular, we will use as an input the following code: `add 1 sub 6 add 3 2`.

## Parsing

The first thing we want to do is to parse the code into an Abstract Syntax Tree (AST). In other words, a structure that enables us to work easily with the code. The AST for the example code provided above would look like

```
 add
  / \
  1  sub
     / \
    6  add
       / \
      3   2
```

The code to do that is the following

```haskell
data Ast
  = Node Op Ast Ast
  | Value Int
instance showAst :: Show Ast where
  show (Node op ast1 ast2) = "(" <> show op <> " " <> show ast1 <> " " <> show ast2 <> ")"
  show (Value i) = show i

data Op
  = Add
  | Sub
instance showOp :: Show Op where
  show Add = "Add"
  show Sub = "Sub"

astParser :: Parser Ast
astParser = do
  skipSpaces
  op <- opParser
  skipSpaces
  ast1 <- valueParser <|> astParser
  skipSpaces
  ast2 <- valueParser <|> astParser
  pure $ Node op ast1 ast2

opParser :: Parser Op
opParser =
  addParser <|> subParser

addParser :: Parser Op
addParser = do
  _ <- string "add"
  pure Add

subParser :: Parser Op
subParser = do
  _ <- string "sub"
  pure Sub

intParser :: Parser Int
intParser = do
  ds <- many1 anyDigit
  case fromString $ fromChars ds of
    Just i -> pure i
    Nothing -> fail "I was expecting an int!"

valueParser :: Parser Ast
valueParser = do
  i <- intParser
  pure $ Value i

```

## Generation / Evaluation

With the AST at our disposal we can either compile to some other language or evaluate the code:

```haskell
generate :: Ast -> String
generate (Value i) = show i
generate (Node Add ast1 ast2) =
  "(" <> generate ast1 <> " + " <> generate ast2 <> ")"
generate (Node Sub ast1 ast2) =
  "(" <> generate ast1 <> " - " <> generate ast2 <> ")"

evaluate :: Ast -> Int
evaluate (Value i) = i
evaluate (Node Add ast1 ast2) =
  evaluate ast1 + evaluate ast2
evaluate (Node Sub ast1 ast2) =
  evaluate ast1 - evaluate ast2
```

## Test Drive

```haskell
input :: String
input =
  "add 1 sub 6 add 3 2"

main :: Effect Unit
main = do
  logShow $ runParser astParser input
  -- (Right (Add 1 (Sub 6 (Add 3 2))))
  logShow $ map generate $ runParser astParser input
  -- (Right "(1 + (6 - (3 + 2)))")
  logShow $ map evaluate $ runParser astParser input
  -- (Right 2)
```

## Outro

If you want to read more, this is what inspired me to take a few steps into the compilers world:

- [The Super Tiny Compiler](https://github.com/jamiebuilds/the-super-tiny-compiler/blob/master/the-super-tiny-compiler.js)
- [Tiny Interepter and Compiler](https://github.com/mgechev/tiny-compiler/blob/master/tiny.js)
- [How to be a compiler — make a compiler with JavaScript](https://medium.com/@kosamari/how-to-be-a-compiler-make-a-compiler-with-javascript-4a8a13d473b4)

---

If you liked the post and want to help spread the word, please make some noise 🤘 But only if you really liked it. Otherwise, please feel free to comment or tweet me with any suggestions or feedback. And please do cause I need help with my FP!
