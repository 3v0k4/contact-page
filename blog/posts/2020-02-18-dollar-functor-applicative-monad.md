---
title: Staring at ($), (<$>), (<*>) and (>>=)
description: Differences and similarities between function application, Functor's `fmap`, Applicative Functor's sequential application and Monad's `bind`
author: Riccardo
tags:
  - Functional Programming
  - Haskell
  - Functor
  - Applicative
  - Monad
---

Recently I've spent some time staring at type signatures. The goal was to develop a better intuition by absorbing their wisdom. Last week it was [Monad's `bind`](https://odone.io/posts/2020-02-03-monad-composes-sequentially.html). This time I've decided to compare the following four:

```hs
($)   ::   (a ->   b) ->   a ->   b
(<$>) ::   (a ->   b) -> f a -> f b
(<*>) :: f (a ->   b) -> f a -> f b
(>>=) ::   (a -> m b) -> m a -> m b
```

## Function Application or [`($)`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:-36-)

```hs
($) :: (a -> b) -> a -> b
```

It takes a funtion from a value of type `a` to a value of type `b`, an `a` and returns `b`. There's only one possible way to implement `($)` which is to apply the funtion to the value of type `a`.

## Functor's `fmap` or [`(<$>)`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:fmap)

```hs
(<$>) :: (a -> b) -> f a -> f b
```

The only difference from the previous is that `a` and `b` exist in a context `f`. For example, we could have an `Int` in a `List` context (i.e. `[Int]`), which means we went from one `Int` to any number of `Int`s. Or we could have an `Int` in a `Maybe` context (i.e. `Maybe Int`), in other words there could be either no `Int`s or just one `Int`. And so on and so forth depending on the semantics of each functor.

Again, it's easy to see how the value of type `a` must feed the function from `a` to `b` to generate the output. The only difference from `($)` is that depending on the semantics of the context `f`, the function will be applied in a different way.

## Applicative Functor's sequential application or [`(<*>)`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:-60--42--62-)

```hs
(<*>) :: f (a -> b) -> f a -> f b
```

In this instance, the function from `a` to `b` has a context `f` too. Therefore, the way the output is calculated depends on both the first and the second `f` (which must be the same `f`).

## Monad's `bind` or [`(>>=)`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:-62--62--61-)

```hs
(>>=) :: (a -> m b) -> m a -> m b
```
This time, the way the function is applied depends only on the second `m`. This is the same situation as for `(<$>)`. But there's one important change: the previous functions could only transform an `a` into a `b`. In the case of `bind`, the funtion decides not only on the `b` but also on the `m`, which must be the same `m` for both.

## Concretely

Let's see the above in action in the context of `Either` which has an instance for Functor, Applicative Functor and Monad. Notice that the instances are defined for `Either e` because the context they provide is around one type, not two. For example, given an `Int` we can provide it an `Either String` context by doing `Either String Int`.

```hs
show 1
--> "1"



-- ($)

show $ 1
--> "1"



-- (<$>)

show <$> Right 1
--> Right "1"

show <$> Left "string"
--> Left "string"

-- Either maps the function only when the value is a `Right`.



-- (<*>)

Right show <*> Right 1
--> Right "1"

Right show <*> Left "string"
--> Left "string"

Left show <*> Right 1
--> Left show

Left show <*> Left "string"
--> Type error: the type on the left should be the same for both `Either`s.

-- Either applies the function only when both values are `Right`.



-- (>>=)

Right 1 >>= (\x -> Right (show x))
--> Right "1"

Left "string" >>= (\x -> Right (show x))
--> Left "string"

Right 1 >>= (\x -> Left (show x))
--> Left "string"

Left "string" >>= (\x -> Left (show x))
--> Left "string"

-- Either binds the function only when the value before `>>=` is a `Right`.
-- Contrarily to the previous cases, `>>=` can decide to return `Left` or `Right`.
```
