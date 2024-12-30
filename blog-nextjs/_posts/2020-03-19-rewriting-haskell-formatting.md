---
title: Rewriting to Haskell–Automatic Formatting
description: Using Ormolu to keep code pretty and stop once for all discussions on cosmetics
author: Riccardo
series: Rewriting to Haskell
tags:
  - Functional Programming
  - Haskell
  - Servant
---

One comment in a pull request about code formatting was enough to convince us we needed a tool to solve the problem once for all!

We decided to go with [Ormolu](https://github.com/tweag/ormolu#editor-integration) because it's really similar to how [Elm Format](https://github.com/avh4/elm-format) behaves. In particular, from the readme, among other things:

> Implementing one “true” formatting style which admits no configuration.

and

> Let some whitespace be programmable. The layout of the input influences the layout choices in the output. This means that the choices between single-line/multi-line layouts in each particular situation are made by the user, not by an algorithm. This makes the implementation simpler and leaves some control to the user while still guaranteeing that the formatted code is stylistically consistent.

To install Ormolu it's enough to run the following:

```bash
stack install ormolu
```

Then formatting can be checked in CI with:

```bash
ormolu -m check **/*.hs
```

Or formatted with:

```bash
ormolu -m inplace **/*.hs
```

Also, [plugins](https://github.com/tweag/ormolu#editor-integration) are available to automate formatting whenever a Haskell file is saved! In VIM it works really well:

![Gif showing Ormolu formatting a Haskell file in a VIM buffer](/images/ormolu.gif)
