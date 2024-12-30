---
title: Testing Bank Kata in PureScript
author: Riccardo
description: Unit testing monadic functions
cover_image: https://cdn-images-1.medium.com/max/1024/1*CQ1bneAfNmhYuocE6f8gxg.jpeg
series: Bank Kata in PureScript
tags:
  - Functional Programming
  - PureScript
---

## Intro

Last week we’ve had some fun solving the [Bank Kata in PureScript](/posts/2019-03-13-bank-kata-in-purescript/). Now it’s time to add some unit tests.

In particular, we are going to test the three main functions of the kata:

```hs
deposit :: Int -> StateT (Array Transaction) Effect Unit

withdraw :: Int -> StateT (Array Transaction) Effect Unit

printStatement :: StateT (Array Transaction) Effect Unit
```

## The Tests

Let’s start with `deposit`:

```hs
deposit :: Int -> StateT (Array Transaction) Effect Unit
deposit amount = do
ts <- lift nowDateTime
let t = Deposit { timestamp: ts, amount: amount }
modify\_ \ts -> ts <> [t]
```

Unfortunately, it uses `Effect`. That means, it does something impure we cannot check in a unit test.

We can fix that easily by changing the type signature into

```hs
deposit
 :: forall m. Monad m
 => Int
 -> StateT (Array Transaction) m Unit
```

In other words, we don’t specify the specific monad (`Effect`) anymore. We just say that `deposit` uses a monad `m` as a base monad for `StateT`.

Sadly, that does not compile. In fact, the type signature is telling a lie. In the body of the function we do `ts <- lift nowDateTime`. As explained in the [previous post](/posts/2019-03-13-bank-kata-in-purescript/), that obliges the function to use `Effect`.

Luckily, this is an easy fix. Instead of using `nowDateTime` in `deposit`, we will just inject it:

```hs
deposit
 :: forall m. Monad m
 => m DateTime
 -> Int
 -> StateT (Array Transaction) m Unit
```

The downside of this refactoring is that we need to change the production code from `deposit 500` to `deposit nowDateTime 500`. The upside is that we can use a unit testable monad now. Not that bad!

Here’s the test

```hs
testDeposit :: Effect Unit
testDeposit = do
 ts <- nowDateTime
 let amount = 1
 expected = Identity [Deposit {amount: amount, timestamp: ts}]
 actual = execStateT (deposit (Identity timestamp) amount) []
 assertEqual { actual: actual, expected: expected }
```

We wrap `timestamp :: DateTime` in the `Identity` monad so that `deposit (Identity timestamp) amount` has type `StateT (Array Transaction) Identity Unit`. That means, `execStateT` returns `Identity (Array Transaction)`.

Testing `withdraw` follows the exact same pattern so we are not going to cover that.

Let’s move to `printStatement`:

```hs
printStatement :: StateT (Array Transaction) Effect Unit
printStatement = do
 s <- gets toStatement
 lift $ log s
```

Here the story is really similar to what we did to `deposit`:

```hs
printStatement :: forall m. Monad m => (String -> m Unit) -> StateT (Array Transaction) m Unit
printStatement logger = do
 s <- gets toStatement
 lift $ logger s
```

And the corresponding unit test:

```hs
testPrintStatementWithTransactions :: Effect Unit
testPrintStatementWithTransactions = do
 timestamp <- nowDateTime
 let d = Deposit { amount: 500, timestamp: timestamp }
 w = Withdraw { amount: 100, timestamp: timestamp }
 state = [d, w]
 expected = "expected string"
 actual = execWriter (execStateT (printStatement \s -> tell s) state)
 assertEqual { actual: actual, expected: expected }
```

Notice that as a base monad we use [`Writer`](https://pursuit.purescript.org/packages/purescript-transformers/4.2.0/docs/Control.Monad.Writer). This monad gives us access to `tell` which allows us to append to an accumulator. That way `printStatement` “writes” the statement in the accumulator instead of the console.

## Show me the Code

Code:

```hs
data Transaction
  = Deposit Info
  | Withdraw Info

derive instance eqTransaction :: Eq Transaction

instance showTransaction :: Show Transaction where
  show (Deposit i) = show i
  show (Withdraw i) = show i

type Info =
  { timestamp :: DateTime
   , amount    :: Int
  }

deposit :: forall m. Monad m => m DateTime -> Int -> StateT (Array Transaction) m Unit
deposit nowDateTime amount = do
  ts <- lift nowDateTime
  let t = Deposit { timestamp: ts, amount: amount }
  modify_ \ts -> ts <> [t]

withdraw :: forall m. Monad m => m DateTime -> Int -> StateT (Array Transaction) m Unit
withdraw nowDateTime amount = do
  ts <- lift nowDateTime
  let t = Withdraw { timestamp: ts, amount: amount }
  modify_ \ts -> ts <> [t]

printStatement :: forall m. Monad m => (String -> m Unit) -> StateT (Array Transaction) m Unit
printStatement logger = do
  s <- gets toStatement
  lift $ logger s

toStatement :: Array Transaction -> String
toStatement =
  fst <<< foldl fnc (Tuple "" 0)
  where
  fnc (Tuple s i) (Deposit d) =
    Tuple (s <> "\n" <> joinWith " " [ show d.timestamp, show d.amount, show $ i + d.amount]) (i + d.amount)
  fnc (Tuple s i) (Withdraw w) =
    Tuple (s <> "\n" <> joinWith " " [ show w.timestamp, "-" <> show w.amount, show $ i - w.amount]) (i - w.amount)

main :: Effect Unit
main = do
  flip evalStateT [] do
    deposit nowDateTime 500
    withdraw nowDateTime 100
    printStatement log
```

Tests:

```hs
main :: Effect Unit
main = do
  testDeposit
  testWithdraw
  testPrintStatementNoTransactions
  testPrintStatementWithTransactions

testDeposit :: Effect Unit
testDeposit = do
  timestamp <- nowDateTime
  let amount = 1
      expected = Identity [ Deposit { amount: amount, timestamp: timestamp } ]
      actual = execStateT (deposit (Identity timestamp) amount) [] 
  assertEqual { actual: actual, expected: expected }

testWithdraw :: Effect Unit
testWithdraw = do
  timestamp <- nowDateTime
  let amount = 1
      expected = Identity [ Withdraw { amount: amount, timestamp: timestamp } ]
      actual = execStateT (withdraw (Identity timestamp) amount) [] 
  assertEqual { actual: actual, expected: expected }

testPrintStatementNoTransactions :: Effect Unit
testPrintStatementNoTransactions = do
  let expected = ""
      actual = execWriter (evalStateT (printStatement \s -> tell s) [])
  assertEqual { actual: actual, expected: expected }

testPrintStatementWithTransactions :: Effect Unit
testPrintStatementWithTransactions = do
  timestamp <- nowDateTime
  let d = Deposit { amount: 500, timestamp: timestamp }
      w = Withdraw { amount: 100, timestamp: timestamp }
      state = [ d, w ]
      expected = "expected string"
      actual = execWriter (evalStateT (printStatement \s -> tell s) state)
  assertEqual { actual: actual, expected: expected }
```

## Outro

If you liked the post and want to help spread the word, please make some noise 🤘 But only if you really liked it. Otherwise, please feel free to comment or tweet me with any suggestions or feedback. And please do cause I need help with my FP!
