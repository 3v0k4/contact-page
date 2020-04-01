---
title: Rewriting to Haskell–Making GHC More Nitpicky
description: Enabling GHC compiler warnings to enforce better code
author: Riccardo
tags:
  - FunctionalProgramming
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

---

GHC by default compiles with some warnings enabled. But there's a ton more. Thus, we decided to turn on several. Also, we have the compiler treat them as errors with `-Wall`. Luckily, [Max Tagher](https://twitter.com/MaxTagher) did all the work for us and prepared a ["Copy-Pastable List"](https://medium.com/mercury-bank/enable-all-the-warnings-a0517bc081c3) with nice explanations for us lazy developers. Thanks Max!

This is what we threw in `package.yaml`:

```diff
 library:
   source-dirs: src
+  ghc-options:
+    # For details on warnings: https://downloads.haskell.org/~ghc/master/users-guide/using-warnings.html
+    # This list taken from https://medium.com/mercury-bank/enable-all-the-warnings-a0517bc081c3
+    # Enable all warnings with -Weverything, then disable the ones we don’t care about
+    - -Weverything
+    - -Werror
+    - -Wno-missing-exported-signatures # missing-exported-signatures turns off the more strict -Wmissing-signatures. See https://ghc.haskell.org/trac/ghc/ticket/14794#ticket
+    - -Wno-missing-import-lists # Requires explicit imports of _every_ function (e.g. ‘$’); too strict
+    - -Wno-missed-specialisations # When GHC can’t specialize a polymorphic function. No big deal and requires fixing underlying libraries to solve.
+    - -Wno-all-missed-specialisations # See missed-specialisations
+    - -Wno-unsafe # Don’t use Safe Haskell warnings
+    - -Wno-safe # Don’t use Safe Haskell warnings
+    - -Wno-missing-local-signatures # Warning for polymorphic local bindings; nothing wrong with those.
+    - -Wno-monomorphism-restriction # Don’t warn if the monomorphism restriction is used
   exposed-modules:
```

Turns out we have been disciplined enough to just need a few edits to fix the new errors. What follows is a non-comprehensive list of the changes we performed together with the warning that was triggered.

`-Wmissing-import-lists`:

```diff
-module Api.Search where
+module Api.Search
+  ( SearchAPI,
+    getResults,
+  )
+where
```

`-Wimplicit-prelude`:

```diff
 import Servant ((:>), Get, Handler, JSON, QueryParam)
+import Prelude
```

`-Wincomplete-patterns`:

```diff
 instance FromJSON Configuration where
   parseJSON (Object x) = do
     -- ...
+  parseJSON _ = fail "was expecting an object"
```

`-Wmissing-exported-signatures`:

```diff
+joinComments :: Query
 joinComments = "LEFT JOIN comments ON posts.id = comments.post_id"

+textOrTitle :: Query
 textOrTitle = "(posts.text ILIKE ? OR posts.title ILIKE ?)"
```

Some warnings can be annoying or even unneeded depending on the use-case, your mileage may vary. Feel free to treat this as a starting point and tweak the options down the line.
