---
title: Functors Compose, Monads Do Not
author: Riccardo
---

<img class="cover" src="https://cdn-images-1.medium.com/max/1024/1*QM3gGl2zlucMsox-1WNirQ.jpeg"></img>

## Functor Composition

Let’s start with a refresher of `map`:

```haskell
map :: forall a b. (a -> b) -> f a -> f b
```

In other words, map takes a function `a -> b` and gives us a function `f a -> f b`. For that reason, we can take any two nested functors (e.g. `Array` and `Maybe`) and run a function on the nested values by putting two `map`s together:

```haskell
v :: Array (Maybe Int)
v = [Just 1, Nothing, Just 3]

f1 :: Int -> String
f1 = show

main :: Effect Unit
main = do
 logShow $ map (map f1) v

-- [(Just "1"),Nothing,(Just "3")]
```

## Monad Composition

This time we want to take a look at `bind`:

```haskell
bind :: forall a b. m a -> (a -> m b) -> m b
```

If we tried to compose the same way we did with functors, we would notice the code does not compile:

```haskell
v :: Array (Maybe Int)
v = [Just 1, Nothing, Just 3]

f2 :: Int -> Array (Maybe String)
f2 i = [Just $ show i]

main :: Effect Unit
main = do
 logShow $ bind v (\x -> bind x f2) -- DOES NOT COMPILE!!
```

The problem here is in the nested `bind`:

```haskell
bind v (\x -> bind x f2)
     ^ Maybe Int
       ^ Int -> Array (Maybe String)
```

In fact, `Maybe Int -> (Int -> Array (Maybe String)) -> ??` is not what `bind` expects: the first argument seems to indicate that `m` is `Maybe` but the second seems to indicate that `m` is `Array`. This does not compile since the monad `m` is supposed to be the same.

To make the program compile we have to make use of a function (i.e. `maybe`) specific to the monad we are dealing with (i.e. `Maybe`):

```haskell
main :: Effect Unit
main = do
 -- logShow $ bind v (\x -> bind x f2) -- DOES NOT COMPILE!!
 logShow $ bind v (maybe (pure Nothing) f2)
```

Or we could use the `MaybeT` monad transformer:

```haskell
v2 :: MaybeT Array Int
v2 = MaybeT [Just 1, Nothing, Just 3]

f3 :: Int -> MaybeT Array String
f3 i = MaybeT [Just $ show i]

main :: Effect Unit
main = do
 --logShow $ bind v (\x -> bind x f2)
 logShow $ bind v (maybe [Nothing] f2)
 logShow $ runMaybeT $ bind v2 f3
```

## Outro

I’ve blatantly copied the content of this blog post out of a talk by Tony Morris. So be sure to check the [original stuff](https://vimeo.com/73648150) out!

If you liked the post and want to help spread the word, please make some noise 🤘 But only if you really liked it. Otherwise, please feel free to comment or tweet me with any suggestions or feedback. And please do cause I need help with my FP!
