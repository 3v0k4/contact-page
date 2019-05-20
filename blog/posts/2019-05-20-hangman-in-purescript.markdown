---
title: Hangman in PureScript
author: Riccardo
description: Interactive Programming in PureScript
cover_image: https://odone.io/images/hangman.jpeg
---

# Hangman in PureScript

An example run

```bash
Insert word to guess
> hello
-----
Try to guess
> hol
h-l--
> hello
You won!
```

The code

```haskell
module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.ReadLine as RL
import Data.String.Yarn (fromChars, toChars) as S
import Data.List.Lazy as L
import Data.String (length) as S

mask :: String -> String -> String
mask word guess =
  let as = S.toChars word :: L.List Char
      bs = S.toChars guess :: L.List Char
      bs' = bs <> L.replicate (L.length as - L.length bs) '-'
      zipper a b = if a == b then a else '-'
  in
      S.fromChars $ L.zipWith zipper as bs'

main :: Effect Unit
main = do
    interface <- RL.createConsoleInterface RL.noCompletion
    let
        lineHandler word guess =
            case word of
                 "" -> do
                    log $ S.fromChars $ L.take (S.length guess) (L.repeat '-')
                    log "Try to guess"
                    RL.setLineHandler interface $ lineHandler guess
                    RL.setPrompt "> " 2 interface
                    RL.prompt interface
                 _ ->
                    if word == guess then do
                        RL.close interface
                        log "You won!"
                    else do
                        log $ mask word guess
                        RL.setPrompt "> " 2 interface
                        RL.prompt interface
    RL.setLineHandler interface $ lineHandler ""
    log "Insert word to guess"
    RL.setPrompt "> " 2 interface
    RL.prompt interface
```
