---
title: Why Monad Composes Operations Sequentially
description: A deeper look at Monad's bind (i.e. >>=) to understand why the type signature implies sequential composition.
author: Riccardo
tags:
  - FunctionalProgramming
  - Monad
---

When learning new stuff, a great strategy is skipping what's unclear at the moment and coming back later. It's a bit like doing crosswords, things get more clear as they are tackled from more than one perspective. Knowing what comes before and what comes after, makes it easier to grasp the concept that stands in between.

I've heard multiple times that monads are like semicolons. In other words, they allow things to happen in a serial fashion. The part that I could not understand was why people kept saying that it's clear by looking at `bind`'s type signature:

```hs
(>>=) :: forall a b. m a -> (a -> m b) -> m b
```

I've been in the dark for a long time, until a few days ago it clicked. So here I am, writing the post my past self would have loved to read.

First of all, let's dissect the type signature a bit:

```hs
(>>=) :: forall a b. m a -> (a -> m b) -> m b
--                   ^ Start
--                           ^ Step
--                                        ^ End
```

We start with a `m a` and using a step function we advance to `m b`. For that to happen we need to apply step to the value `a` that is in `m a`.

It's important to remind ourselves what `m a` means: a monadic operation that eventually computes a result of type `a`. In other words, until the result is not "produced", there's no way the step function can be applied. That is why bind sequences operations!

Using the Maybe monad that would look like the following:

```hs
sequence :: Maybe Int
sequence = do
  x <- somethingThatReturnsMaybe
  y <- somethingElseThatReturnsMaybe x
  y+1
```

The `do` notation gets translated to:

```hs
sequence :: Maybe Int
sequence =
  somethingThatReturnsMaybe       >>= \x ->
  somethingElseThatReturnsMaybe x >>= \y ->
  y+1
```

Both `somethingThatReturnsMaybe` and `somethingElseThatReturnsMaybe` use Maybe because in some cases they succeed (return Just a value) in some others they fail (return Nothing). In the former case, `somethingElseThatReturnsMaybe` to  run needs `somethingThatReturnsMaybe` to produce the value `x`. In the latter, the Monad instance for Maybe makes `bind` short-circuit and early-return a Nothing.
