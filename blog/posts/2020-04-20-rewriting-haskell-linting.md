---
title: Rewriting to Haskell–Linting
description: Using HLint to improve the code in Stream
author: Riccardo
tags:
  - Functional Programming
  - Haskell
  - Servant
---

This is part of a series:

- [Rewriting to Haskell–Intro](https://odone.io/posts/2020-02-26-rewriting-haskell-intro.html)
- [Rewriting to Haskell–Project Setup](https://odone.io/posts/2020-03-03-rewriting-haskell-setup.html)
- [Rewriting to Haskell–Deployment](https://odone.io/posts/2020-03-14-rewriting-haskell-server.html)
- [Rewriting to Haskell–Automatic Formatting](https://odone.io/posts/2020-03-19-rewriting-haskell-formatting.html)
- [Rewriting to Haskell–Configuration](https://odone.io/posts/2020-03-23-rewriting-haskell-configuration.html)
- [Rewriting to Haskell–Standing on the shoulders of Rails](https://odone.io/posts/2020-03-30-rails.html)
- [Rewriting to Haskell–Making GHC More Nitpicky](https://odone.io/posts/2020-04-06-ghc-options.html)
- [Rewriting to Haskell–Testing](https://odone.io/posts/2020-04-13-rewriting-haskell-testing.html)

---

I bet most of us are not surrounded by as many haskellers as we would wish. That means we need to treasure every ounce of feedback we can get. Turns out a linter is a great way of squeezing some more.

We recently added [HLint](https://hackage.haskell.org/package/hlint) to Stream. What follows are some of the changes we've made thanks to its suggestions.

Unused LANGUAGE pragma:

```diff
 {-# LANGUAGE DataKinds #-}
-{-# LANGUAGE OverloadedStrings #-}
 {-# LANGUAGE TypeOperators #-}
```

Use infix:

```diff
-toChannels channel = intersect activeChannels [channel]
+toChannels channel = activeChannels `intersect` [channel]
```

Redundant bracket:

```diff
-  toField (Disabled) = toField ("NULL" :: Text)
+  toField Disabled = toField ("NULL" :: Text)
```

Redundant $:

```diff
-  toField NoQuery = toField $ ("%" :: Text)
+  toField NoQuery = toField ("%" :: Text)
```

Redundant do:

```diff
-findPosts connection searchQuery searchQuantity searchComments searchChannels searchLastId = do
+findPosts connection searchQuery searchQuantity searchComments searchChannels searchLastId =
   query connection (sqlQuery searchLastId) (searchQuery, searchQuery, searchComments, searchChannels, searchQuantity)
```

Eta reduce:

```diff
-  random g = randomR (minBound, maxBound) g
+  random = randomR (minBound, maxBound)
```

```diff
-server configuration connection = getSearchResults configuration connection
+server = getSearchResults
```

Use <$> (instead of `>>= pure`):

```diff
-  updatedAt <- randomUTCTime >>= pure . utcToLocalTime (read "UTC")
+  updatedAt <- utcToLocalTime (read "UTC") <$> randomUTCTime
```

```diff
-  createdAt <- maybe randomUTCTime pure mCreatedAt >>= pure . utcToLocalTime (read "UTC")
+  createdAt <- utcToLocalTime (read "UTC") <$> maybe randomUTCTime pure mCreatedAt
```

Hints provided by HLint should be taken as such. In our case, we took what felt right and left the rest out. This is why we do not run HLint in CI. However, there are [many ways of integrating it](https://github.com/ndmitchell/hlint#integrations) to get the best out without the pain of broken builds. Be sure to take a look at the readme cause there's a lot of additional goodness (e.g. [automatically applying hints](https://github.com/ndmitchell/hlint#automatically-applying-hints)).
