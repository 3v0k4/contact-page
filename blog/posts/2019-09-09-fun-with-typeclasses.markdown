---
title: Fun with Typeclasses
description: Ever heard of `Enum`, `Bounded`, `BoundedEnum` and `CyclicEnum`?
author: Riccardo
cover_image: https://odone.io/images/typeclasses.jpg
---

I've recently started reading ["Haskell in Depth"](https://www.manning.com/books/haskell-in-depth). The first part of the book covers the basics of the language. Here, among other cool things I've found an interesting example using two existing typeclasses (i.e. `Enum`, `Bounded`) and two new typeclasses that seem to make a lot of sense (i.e. `BoundedEnum`, `CyclicEnum`).

Let's see that stuff in action. First of all, we need a type to play with:

```hs
data Direction
    = North
    | East
    | South
    | West
    deriving (Show, Enum, Bounded, BoundedEnum, Eq, CyclicEnum)
```

The instance of `Enum` enables us to enumerate the values of type `Direction`. In other words, we can move to the next value or previous value by using `succ` and `pred`. Not only that, by making `Direction` enumerable, we can use the `..` operator:

```hs
main :: IO ()
main = do
  print $ succ North
  -- East

  print $ pred East
  -- North

  print [North ..]
  -- [North,East,South,West]

  print [East ..]
  -- [East,South,West]

  print [East .. South]
  -- [East,South]
```

The instance `Bounded` allows us to generically call the lower-bound and upper-bound `minBound` and `maxBound`:

```hs
main :: IO ()
main = do
  let
      allDirections :: [Direction]
      allDirections = [minBound .. maxBound]
  print allDirections
  -- [North,East,South,West]
```

Now, in Haskell an `Enum` could be not `Bounded` and the other way around. In case a type is both, it makes sense to define a `BoundedEnum` typeclass and instance. That way, we can abstract the range `[minBound .. maxBound]`:

```hs
class (Enum a, Bounded a) => BoundedEnum a where
    range :: [a]
    range = enumFrom minBound

main :: IO ()
main = do
  let
      allDirections' :: [Direction]
      allDirections' = range
  print $ allDirections'
  -- [North,East,South,West]
```

There is still a problem though. In fact, the following raises a runtime exception:

```hs
main :: IO ()
main = do
  print $ pred North
  -- "tried to take `pred' of first tag in enumeration"
```

As a matter of fact, the enumeration does not wrap around and `North` is the first value in the enum! We can solve that by defining a `CyclicEnum` typeclass and instance:

```hs
class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred d
        | d == minBound = maxBound
        | otherwise = pred d

    csucc :: a -> a
    csucc d
        | d == maxBound = minBound
        | otherwise = succ d

main :: IO ()
main = do
  print $ cpred North
  -- West
```