---
title: A State Monad in PureScript
author: Riccardo
description: Implementing and using the State Monad
cover_image: https://odone.io/images/state.jpg
tags:
  - Functional Programming
  - PureScript
---

Functional programmers love pure functions. Unfortunately, some things are inherently stateful. However, that doesn't mean we cannot tackle them in purely functional languages. It just means they are more naturally modelled using mutable state. Or at least, that's what a background in object oriented code could make us think.

One thing that is easily modelled with mutable state is a stack. This is how [Wikipedia describes it](https://en.wikipedia.org/wiki/Stack_(abstract_data_type))

> In computer science, a stack is an abstract data type that serves as a collection of elements, with two principal operations:
>
> - push, which adds an element to the collection, and
> - pop, which removes the most recently added element that was not yet removed.

And this is how it looks in code:

```haskell
type Stack = List Int

push :: Int -> Stack -> Stack
push x st = x : st

pop :: Stack -> Tuple (Maybe Int) Stack
pop xs = Tuple (head xs) (drop 1 xs)

main :: Effect Unit
main = do
  let stack           = 3 : 2 : 1 : Nil
      stack4          = push 4 stack
      Tuple m4 stack3 = pop stack4
      Tuple m3 stack2 = pop stack3
  logShow stack
  -- (3 : 2 : 1 : Nil)
  logShow stack4
  -- (4 : 3 : 2 : 1 : Nil)
  logShow m4
  -- (Just 4)
  logShow stack3
  -- (3 : 2 : 1 : Nil)
  logShow m3
  -- (Just 3)
  logShow stack2
  -- (2 : 1 : Nil)
```

If we rewrite `push` and we put it side by side with `pop`

```haskell
push :: Int -> Stack -> Tuple Unit Stack
push x st = Tuple unit (x : st)

pop :: Stack -> Tuple (Maybe Int) Stack
pop xs = Tuple (head xs) (drop 1 xs)
```

we can notice a symmetry. In particular, we can image `push` and `pop` as operations that produce a tuple of results: an intermediate value and the new state of the stack.

When `pop`ping the intermediate value is the popped value and the new state of the stack is the old stack without the popped value.

When `push`ing the intermediate value is nothing (we use `unit` to model that) and the new state of the stack is the old stack with the pushed value added.

This intuition of a function that produces an intermediate value and a new state is captured by the State Monad. Let's implement one!

## Implementing the Stat3 Monad

In this section we are going to implement a State Monad. We will use `4`s and `3`s in place of `a`s and `e`s in names (e.g. `Stat3` vs `State`). Since we are going to use the same api as the `State` monad from [Control.Monad.State](https://pursuit.purescript.org/packages/purescript-transformers/4.2.0/docs/Control.Monad.State#t:State), that will avoid name clashes.

We start by defining `Stat3`:

```haskell
newtype Stat3 s v = Stat3 (s -> Tuple v s)
```

In other words, a wrapper around a function that goes from current state to tuple of intermediate value and new state. We've used two type variables to be able to control the type of the state (`s`) and the type of the intermediate value (`v`).

That looks really similar to the types of `pop` (`Stack -> Tuple (Maybe Int) Stack`) and `push` (`... -> Stack -> Tuple Unit Stack`).

Then we define a function to "unwrap" `s -> Tuple v s` and run it with an initial state `s`:

```haskell
runStat3 :: forall s v. Stat3 s v -> s -> Tuple v s
runStat3 (Stat3 g) s = g s
```

To be able to write declarative code and match the api of [`State`](https://pursuit.purescript.org/packages/purescript-transformers/4.2.0/docs/Control.Monad.State#t:State) we implement all the required typeclasses until `Monad`.

In particular, we implement the typeclasses on `Stat3 s` and not `Stat3`. That's because they work on type constructors of kind `* -> *` and not `* -> * -> *`.

```haskell
instance functorStat3 :: Functor (Stat3 s) where
    -- map :: forall a b. (a -> b) -> f a -> f b
    map g f = Stat3 (\s -> let Tuple v s' = runStat3 f s in Tuple (g v) s')

instance applyStat3 :: Functor (Stat3 s) => Apply (Stat3 s) where
    -- apply :: forall a b. f (a -> b) -> f a -> f b
    apply fg f = Stat3 (\s -> let Tuple g s'  = runStat3 fg s
                                  Tuple v s'' = runStat3 f s' in Tuple (g v) s'')

instance applicativeStat3 :: Apply (Stat3 s) => Applicative (Stat3 s) where
    -- pure :: forall a. a -> f a
    pure v = Stat3 (\s -> Tuple v s)

instance bindStat3 :: Apply (Stat3 s) => Bind (Stat3 s) where
    -- bind :: forall a b. m a -> (a -> m b) -> m b
    bind m g = Stat3 (\s -> let Tuple v s' = runStat3 m s in runStat3 (g v) s')
```

Now we can write

```haskell
pushStat3 :: Int -> Stat3 (List Int) Unit
pushStat3 x = Stat3 (\s -> Tuple unit (x : s))

popStat3 :: Stat3 (List Int) (Maybe Int)
popStat3 = Stat3 (\s -> Tuple (head s) (drop 1 s))

m4nip :: Stat3 (List Int) Unit
m4nip = do
    pushStat3 4
    _ <- popStat3
    _ <- popStat3
    pure unit

main :: Effect Unit
main = do
  logShow $ runStat3 m4nip (3 : 2 : 1 : Nil)
  -- (Tuple unit (2 : 1 : Nil))
```

We can improve `pushStat3` and `popStat3` as follows:

```haskell
g3t :: forall s. Stat3 s s
g3t = Stat3 (\s -> Tuple s s)

m0dify_ :: forall s. (s -> s) -> Stat3 s Unit
m0dify_ g = do
    s <- g3t
    Stat3 (\s -> Tuple unit (g s))

popStat3 :: Stat3 (List Int) (Maybe Int)
popStat3 = do
    xs <- g3t
    m0dify_ $ drop 1
    pure $ head xs

pushStat3 :: Int -> Stat3 (List Int) Unit
pushStat3 x = m0dify_ (\s -> x : s)
```

## Using the Real™ State Monad

Using the [`State` Monad](https://pursuit.purescript.org/packages/purescript-transformers/4.2.0/docs/Control.Monad.State#t:State) is as easy as replacing `3`s with `e`s and `4`s with `a`s: 

```haskell
pushState :: Int -> State (List Int) Unit
pushState x = modify_ (\s -> x : s)

popState :: State (List Int) (Maybe Int)
popState = do
    xs <- get
    modify_ $ drop 1
    pure $ head xs

manip :: State (List Int) Unit
manip = do
    pushState 4
    _ <- popState
    _ <- popState
    pure unit

main :: Effect Unit
main = do
  logShow $ runState manip (3 : 2 : 1 : Nil)
  -- (Tuple unit (2 : 1 : Nil))
```

## The Whole Code

```haskell
module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Data.List
import Data.Tuple
import Data.Maybe
import Control.Monad.State

type Stack = List Int

push :: Int -> Stack -> Stack
push x st = x : st

pop :: Stack -> Tuple (Maybe Int) Stack
pop xs = Tuple (head xs) (drop 1 xs)

newtype Stat3 s v = Stat3 (s -> Tuple v s)

runStat3 :: forall s v. Stat3 s v -> s -> Tuple v s
runStat3 (Stat3 g) s = g s

instance functorStat3 :: Functor (Stat3 s) where
    -- map :: forall a b. (a -> b) -> f a -> f b
    map g f = Stat3 (\s -> let Tuple v s' = runStat3 f s in Tuple (g v) s')

instance applyStat3 :: Functor (Stat3 s) => Apply (Stat3 s) where
    -- apply :: forall a b. f (a -> b) -> f a -> f b
    apply fg f = Stat3 (\s -> let Tuple g s'  = runStat3 fg s
                                  Tuple v s'' = runStat3 f s' in Tuple (g v) s'')

instance applicativeStat3 :: Apply (Stat3 s) => Applicative (Stat3 s) where
    -- pure :: forall a. a -> f a
    pure v = Stat3 (\s -> Tuple v s)

instance bindStat3 :: Apply (Stat3 s) => Bind (Stat3 s) where
    -- bind :: forall a b. m a -> (a -> m b) -> m b
    bind m g = Stat3 (\s -> let Tuple v s' = runStat3 m s in runStat3 (g v) s')

pushSt4t3 :: Int -> Stat3 (List Int) Unit
pushSt4t3 x = Stat3 (\s -> Tuple unit (x : s))

popSt4t3 :: Stat3 (List Int) (Maybe Int)
popSt4t3 = Stat3 (\s -> Tuple (head s) (drop 1 s))

g3t :: forall s. Stat3 s s
g3t = Stat3 (\s -> Tuple s s)

m0dify_ :: forall s. (s -> s) -> Stat3 s Unit
m0dify_ g = do
    s <- g3t
    Stat3 (\s -> Tuple unit (g s))

popStat3 :: Stat3 (List Int) (Maybe Int)
popStat3 = do
    xs <- g3t
    m0dify_ $ drop 1
    pure $ head xs

pushStat3 :: Int -> Stat3 (List Int) Unit
pushStat3 x = m0dify_ (\s -> x : s)

m4nip :: Stat3 (List Int) Unit
m4nip = do
    pushStat3 4
    _ <- popStat3
    _ <- popStat3
    pure unit

pushState :: Int -> State (List Int) Unit
pushState x = modify_ (\s -> x : s)

popState :: State (List Int) (Maybe Int)
popState = do
    xs <- get
    modify_ $ drop 1
    pure $ head xs

manip :: State (List Int) Unit
manip = do
    pushState 4
    _ <- popState
    _ <- popState
    pure unit

main :: Effect Unit
main = do
  let stack           = 3 : 2 : 1 : Nil
      stack4          = push 4 stack
      Tuple m4 stack3 = pop stack4
      Tuple m3 stack2 = pop stack3
    logShow stack
  -- (3 : 2 : 1 : Nil)
  logShow stack4
  -- (4 : 3 : 2 : 1 : Nil)
  logShow m4
  -- (Just 4)
  logShow stack3
  -- (3 : 2 : 1 : Nil)
  logShow m3
  -- (Just 3)
  logShow stack2
  -- (2 : 1 : Nil)
  logShow $ runStat3 manip (3 : 2 : 1 : Nil)
  -- (2 : 1 : Nil)
  logShow $ runState manip' (3 : 2 : 1 : Nil)
  -- (2 : 1 : Nil)

```
