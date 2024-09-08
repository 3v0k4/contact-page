---
title: Folding Trees in PureScript
author: Riccardo
description: Transforming the structure of a tree by employing recursive functions, the Foldable typeclass and the State Monad
cover_image: https://odone.io/images/folding-trees.jpg
tags:
  - Functional Programming
  - PureScript
---

Let's say we wanted to perform two operations on a tree:

- count the number of leaves
- transform it to a list

In this post we will perform both by employing three different strategies:

- recursive functions
- using the Foldable typeclass
- using the State Monad

## The Tree Type

```hs
data Tree a
    = Leaf a
    | Node (Tree a) (Tree a)

instance showTree :: Show a => Show (Tree a) where
    show (Leaf x)   = "(Leaf " <> show x <> ")"
    show (Node l r) = "(Node " <> show l <> " " <> show r <> ")"

exampleTree :: Tree Char
exampleTree =
    Node
      (Node (Leaf 'a') (Leaf 'b'))
      (Leaf 'c')

main :: Effect Unit
main = do
  logShow exampleTree
  -- (Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c'))
```

## Recursive Functions

PureScript is a purely functional programming language and `Tree a` is a recursive type: recursive functions are a perfect fit.

```hs
countTreeRec :: forall a. Tree a -> Int
countTreeRec tree =
    go 0 tree
    where
          go i (Leaf _)   = i + 1
          go i (Node l r) = go i l + go i r

toListRec :: forall a. Tree a -> List a
toListRec tree =
    go Nil tree
    where
          go xs (Leaf x)   = xs <> Cons x Nil
          go xs (Node l r) = go xs l <> go xs r

main :: Effect Unit
main = do
  logShow $ countTreeRec exampleTree
  -- 3
  logShow $ toListRec exampleTree
  -- ('a' : 'b' : 'c' : Nil)
```

The functions do what they are supposed to do. However, their shape is really similar. The only differences between `countTreeRec` and `toListRec` are:

- the initial value passed to the `go` function (i.e. `0` vs `Nil`)
- the calculation in the base case of `go` (i.e. `i + 1` vs `xs <> Cons x Nil`)
- the way the recursive case combines the result of the recursive calls (i.e. `+` vs `<>`)

What's described above is exactly what the Foldable typeclass captures. Let's see how that looks in code.

## Using the Foldable Typeclass

The Foldable typeclass captures the idea of "folding" a structure into another one.

```hs
instance foldableTree :: Foldable Tree where
    -- foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
    foldMap g (Leaf x)   = g x
    foldMap g (Node l r) = foldMap g l <> foldMap g r

    foldr g = foldrDefault g
    foldl g = foldlDefault g

countTreeFold :: forall a. Tree a -> Int
countTreeFold tree =
    count
    where Additive count = foldMap (\_ -> Additive 1) tree

toListFold :: forall a. Tree a -> List a
toListFold tree =
    foldMap (\x -> Cons x Nil) tree

main :: Effect Unit
main = do
  logShow $ countTreeFold exampleTree
  -- 3
  logShow $ toListFold exampleTree
  -- ('a' : 'b' : 'c' : Nil)
```

In this case, we could have used `foldr` or `foldl` to achieve the same results. But `foldMap` is a tad more elegant. The way it works is simple:

- It first runs each element of the tree through the function passed to it (i.e. `(\_ -> Additive 1)` vs `(\x -> Cons x Nil)`. That function must transform each element of the tree into a Monoid
- It combines all of the Monoids of the tree using the `<>` operator. Since `<>` is implemented as `+` for `Additive` and `<>` is implemented as `Cons` for `List`, everything works as before.

Try to compare `countTreeFold` vs `countTreeRec` and `toListFold` vs `toListRec`.

## Using the State Monad

The foldable trick is totally cool. But why not go overkill implementing and using a State Monad?

```hs
newtype State s a = State (s -> Tuple a s)

runState :: forall s a. State s a -> s -> Tuple a s
runState (State s) a = s a

instance functorState :: Functor (State s) where
    -- map :: forall a b. (a -> b) -> f a -> f b
    map g f = State (\s -> let Tuple a s' = runState f s in Tuple (g a) s')

instance applyState :: Functor (State s) => Apply (State s) where
    -- apply :: forall a b. f (a -> b) -> f a -> f b
    apply fg f = State (\s -> let Tuple g s'  = runState fg s
                                  Tuple a s'' = runState f s' in Tuple (g a) s'')

instance applicativeState :: Apply (State s) => Applicative (State s) where
    -- pure :: forall a. a -> f a
    pure a = State (\s -> Tuple a s)

instance bindState :: Apply (State s) => Bind (State s) where
    -- bind :: forall a b. m a -> (a -> m b) -> m b
    bind m mg = State (\s -> let Tuple a s' = runState m s in runState (mg a) s')

addOne :: State Int Int
addOne = State (\s -> Tuple s (s+1))

countTreeState :: forall a. Tree a -> State Int (Tree Int)
countTreeState (Leaf _)   = Leaf <$> addOne
countTreeState (Node l r) = Node <$> countTreeState l <*> countTreeState r

appendValue :: forall a. a -> State (List a) a
appendValue x = State (\s -> Tuple x (s <> Cons x Nil))

toListState :: forall a. Tree a -> State (List a) (Tree a)
toListState (Leaf x)   = Leaf <$> appendValue x
toListState (Node l r) = Node <$> toListState l <*> toListState r

main :: Effect Unit
main = do
  logShow $ snd $ runState (countTreeState exampleTree) 0
  -- 3
  logShow $ snd $ runState (toListState exampleTree) Nil
  -- ('a' : 'b' : 'c' : Nil)
```

I'm gonna cover `State` in a future post, so keep tuned!

## The Whole Code


```hs
module Main where

import Prelude (class Applicative, class Apply, class Bind, class Functor, class Show, Unit, discard, show, ($), (+), (<$>),
 (<*>), (<>))
import Effect (Effect)
import Effect.Console (logShow)
import Data.Foldable
import Data.List (List(..), foldMap)
import Data.Monoid.Additive (Additive(..))
import Data.Tuple (Tuple(..), snd)

data Tree a
    = Leaf a
    | Node (Tree a) (Tree a)

instance showTree :: Show a => Show (Tree a) where
    show (Leaf x)   = "(Leaf " <> show x <> ")"
    show (Node l r) = "(Node " <> show l <> " " <> show r <> ")"

exampleTree :: Tree Char
exampleTree =
    Node
      (Node (Leaf 'a') (Leaf 'b'))
      (Leaf 'c')

countTreeRec :: forall a. Tree a -> Int
countTreeRec tree =
    go 0 tree
    where
          go i (Leaf _)   = i + 1
          go i (Node l r) = go i l + go i r

toListRec :: forall a. Tree a -> List a
toListRec tree =
    go Nil tree
    where
          go xs (Leaf x)   = xs <> Cons x Nil
          go xs (Node l r) = go xs l <> go xs r

instance foldableTree :: Foldable Tree where
    -- foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
    foldMap g (Leaf x)   = g x
    foldMap g (Node l r) = foldMap g l <> foldMap g r

    foldr g = foldrDefault g
    foldl g = foldlDefault g

countTreeFold :: forall a. Tree a -> Int
countTreeFold tree =
    count
    where Additive count = foldMap (\_ -> Additive 1) tree

toListFold :: forall a. Tree a -> List a
toListFold tree =
    foldMap (\x -> Cons x Nil) tree

newtype State s a = State (s -> Tuple a s)

runState :: forall s a. State s a -> s -> Tuple a s
runState (State s) a = s a

instance functorState :: Functor (State s) where
    -- map :: forall a b. (a -> b) -> f a -> f b
    map g f = State (\s -> let Tuple a s' = runState f s in Tuple (g a) s')

instance applyState :: Functor (State s) => Apply (State s) where
    -- apply :: forall a b. f (a -> b) -> f a -> f b
    apply fg f = State (\s -> let Tuple g s'  = runState fg s
                                  Tuple a s'' = runState f s' in Tuple (g a) s'')

instance applicativeState :: Apply (State s) => Applicative (State s) where
    -- pure :: forall a. a -> f a
    pure a = State (\s -> Tuple a s)

instance bindState :: Apply (State s) => Bind (State s) where
    -- bind :: forall a b. m a -> (a -> m b) -> m b
    bind m mg = State (\s -> let Tuple a s' = runState m s in runState (mg a) s')

addOne :: State Int Int
addOne = State (\s -> Tuple s (s+1))

countTreeState :: forall a. Tree a -> State Int (Tree Int)
countTreeState (Leaf _)   = Leaf <$> addOne
countTreeState (Node l r) = Node <$> countTreeState l <*> countTreeState r

appendValue :: forall a. a -> State (List a) a
appendValue x = State (\s -> Tuple x (s <> Cons x Nil))

toListState :: forall a. Tree a -> State (List a) (Tree a)
toListState (Leaf x)   = Leaf <$> appendValue x
toListState (Node l r) = Node <$> toListState l <*> toListState r

main :: Effect Unit
main = do
  logShow exampleTree
  -- (Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c'))
  logShow $ countTreeRec exampleTree
  -- 3
  logShow $ toListRec exampleTree
  -- ('a' : 'b' : 'c' : Nil)
  logShow $ countTreeFold exampleTree
  -- 3
  logShow $ toListFold exampleTree
  -- ('a' : 'b' : 'c' : Nil)
  logShow $ snd $ runState (countTreeState exampleTree) 0
  -- 3
  logShow $ snd $ runState (toListState exampleTree) Nil
  -- ('a' : 'b' : 'c' : Nil)
```
