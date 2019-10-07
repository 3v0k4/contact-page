---
title: Playing with Fmt
description: Using Fmt to format stuff.
author: Riccardo
cover_image: https://odone.io/images/play.jpg
---

```hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Data.Text as T
import Data.Text.IO as T
import Fmt

data Person = Person
    { firstName :: Text
    , lastName :: Text
    }
    deriving (Show)

instance Buildable Person where
    build person =
        (toTitle $ firstName person) |+ 
            ", " +|
            (toTitle $ lastName person)
            |+ ""

main :: IO ()
main = do
    let name = "mario" :: Text
        person = Person "mario" "mario"
        people = 
            [ Person "mario" "mario"
            , Person "luigi" "mario"
            ]

    T.putStrLn $ "hello " +| name |+ "!"
    -- hello mario!

    T.putStrLn $ "hello " +| toTitle name |+ "!"
    -- hello Mario!

    T.putStrLn $ "hello " +| padBothF 10 '=' name |+ "!"
    -- hello ===mario==!

    T.putStrLn $ "hello " +| whenF False (build name) |+ "!"
    -- hello !

    T.putStrLn $ "hello " +|| person ||+ "!"
    -- hello Person {firstName = "mario", lastName = "mario"}!

    T.putStrLn $ "hello " +| person |+ "!"
    -- hello Mario, Mario!

    T.putStrLn $ "hello " +| people |+ "!"
    -- hello [Mario, Mario,Luigi, Mario]!

    T.putStrLn $ fmt $ unlinesF people
    -- Mario, Mario
    -- Luigi, Mario

    T.putStrLn $ fmt $ jsonListF people
    -- [
    --   Mario, Mario
    -- , Luigi, Mario
    -- ]

    T.putStrLn $ fmt $ blockListF people
    -- - Mario, Mario
    -- - Luigi, Mario
```

And yeah, the name is actually [Mario Mario](https://en.wikipedia.org/wiki/Mario#Surname)!