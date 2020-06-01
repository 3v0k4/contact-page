---
title: RecordDotSyntax in Haskell
description: Records in Haskell are troublesome but the Dot (syntax) is coming soon
published: true
author: Riccardo
tags:
  - Functional Programming
  - Haskell
  - PureScript
---

I started studying hardcore functional programming by fiddling with PureScript. Back then, using records felt natural since they are first-class citizens in the language:

```hs
-- PureScript

type Person = { name :: String, age :: Int }
type Company = { name :: String, owner :: Person }

display :: Company -> String
display c = c.name <> " is run by " <> c.owner.name

nameAfterOwner :: Company -> Company
nameAfterOwner c = c{name = c.owner.name <> "'s Company"}
```

When I switched to Haskell it took me ages to get used to the quirks of its record syntax:

```hs
-- Haskell

data Person = Person { personName :: String, personAge :: Int }
-- ^ Define a new type.
--            ^ Add constructor.
--                     ^ Prefix.
--                                           ^ Prefix.
-- Haskell automatically created
-- Person :: String -> Int -> Person
-- personName :: Person -> String
-- personAge :: Person -> Int

data Company = Company { companyName :: String, companyOwner :: Person }

-- Field names are prefixed, otherwise the program would not
-- compile because of the overlapping `name` getters.

display :: Company -> String
display c = companyName c <> " is run by " <> personName (companyOwner c)

nameAfterOwner :: Company -> Company
nameAfterOwner c = c {companyName = personName (companyOwner c) <> "'s Company"}
```

Of course some type system or category theory trickery would solve that but it just does not feel right for this specific use-case.

Luckily, the [RecordDotSyntax proposal](https://github.com/ghc-proposals/ghc-proposals/pull/282) has been accepted. We just need to hang tight while it gets implemented. But if you are like me and cannot wait to start using it, a [preprocessor](https://github.com/ndmitchell/record-dot-preprocessor) is available today!

In other words, using record-dot-preprocessor, the following code is valid Haskell:

```hs
-- Haskell

data Company = Company {name :: String, owner :: Person}
data Person = Person {name :: String, age :: Int}

display :: Company -> String
display c = c.name ++ " is run by " ++ c.owner.name

nameAfterOwner :: Company -> Company
nameAfterOwner c = c{name = c.owner.name ++ "'s Company"}
```

PureScript has a ton of other niceties. ["Differences from Haskell"](https://github.com/purescript/documentation/blob/master/language/Differences-from-Haskell.md) is a great read on that account.

---

What's your experience with records in Haskell? Hyped up about RecordDotSyntax? Let's talk on [Twitter](https://twitter.com/RiccardoOdone)!
