---
title: Bank Kata in PureScript
author: Riccardo
description: Purely functional state mutations in PureScript
cover_image: https://cdn-images-1.medium.com/max/1024/1*wy9M28S1h8KHOav3b2DBBw.jpeg
---

## Intro

I suck at FP and I desperately need some feedback from you. So please, do not get mad at the code. And double please share feedback if you got any!!

## The Kata

Let’s first introduce the kata by copy / pasting from the awesome [Kata-Log](http://kata-log.rocks/banking-kata):

> Write a class Account that offers the following methods void deposit(int) void withdraw(int) String printStatement()

An example statement would be:

```
Date Amount Balance
24.12.2015 +500 500
23.8.2016 -100 400
```

## Types

`void deposit(int)` and `void withdraw(int)` are impure functions. In fact, they accept an `int` and return `void`. The only way they can do anything useful is to mutate some state.

`String printStatement()` is impure too. As a matter of fact, it returns a string out of nothing. The only way for it to do anything useful is to access some state. In this post, I’ll implement `printStatement` as if it was `void printStatement()`. That is, the function will print the statement in the console. The reason is that I don’t know how to code it otherwise.

One way to read / write state in PureScript is using [the state monad transformer](https://pursuit.purescript.org/packages/purescript-transformers/4.2.0/docs/Control.Monad.State.Trans#t:StateT) (`StateT`).

Therefore, we will use the following types:

```haskell
deposit :: Int -> StateT (Array Transaction) Effect Unit

withdraw :: Int -> StateT (Array Transaction) Effect Unit

printStatement :: StateT (Array Transaction) Effect Unit
```

In other words, our three functions will do their thing in the `StateT (Array Transaction) Effect Unit` environment. In simpler words, each function will be able to manipulate an array of transactions (state), write to console or get datetimes (monadic operations in `Effect`) and return nothing (`Unit`) at the end.

And here we have the type for `Transaction`:

```haskell
data Transaction
 = Deposit Info
 | Withdraw Info

type Info =
 { timestamp :: DateTime
 , amount :: Int
 }
```

## Implementation

Let’s start with `deposit`:

```haskell
deposit :: Int -> StateT (Array Transaction) Effect Unit
deposit amount = do
 ts <- lift nowDateTime
 let t = Deposit { timestamp: ts, amount: amount }
 modify_ \ts -> ts <> [t]
```

Since we are in a monadic environment (`StateT (Array Transaction) Effect Unit`), we open the function with a `do`.

Then we use `nowDateTime :: Effect DateTime` to get the current datetime. The only catch here is that we need to first `lift nowDateTime` in `StateT (Array Transaction) Effect Unit`. That is because in a `do` block each monadic operation (i.e. non `let`s) must all use the same monad. In this case, that means that both `lift nowDateTime` and `modify_ \ts -> ts <> [t]` have type `StateT (Array Transaction) Effect a`.

After that, a deposit with correct `timestamp` and `amount` is assigned to `t`.

Lastly, [`modify_`](https://pursuit.purescript.org/packages/purescript-transformers/4.2.0/docs/Control.Monad.State#v:modify_) is used to access the current state `ts`(array of transactions) by appending the new transaction `t`.

`withdraw` is almost the same:

```haskell
withdraw :: Int -> StateT (Array Transaction) Effect Unit
withdraw amount = do
 ts <- lift nowDateTime
 let t = Withdraw { timestamp: ts, amount: amount }
 modify_ \ts -> ts <> [t]
```

Finally, we have `printStatement`:

```haskell
printStatement :: StateT (Array Transaction) Effect Unit
printStatement = do
 s <- gets toStatement
 lift $ log s
```

The first line uses [`gets`](https://pursuit.purescript.org/packages/purescript-transformers/4.2.0/docs/Control.Monad.State#v:gets) to take the state (array of transactions) and run it through `toStatement :: Array Transaction -> String`. That means `gets toStatement` has type `Effect String` and `s` has type `String`.

The last line lifts `log s :: Effect Unit` in `StateT (Array Transaction) Effect Unit`. In other words, it prints `s` to the console.

The implementation of `toStatement` is not that important. Here is an example of that:

```haskell
toStatement :: Array Transaction -> String
toStatement =
 fst <<< foldl fnc (Tuple "" 0)
 where
 fnc (Tuple s i) (Deposit d) =
 Tuple (s <> "\n" <> joinWith " " [show d.timestamp, show d.amount, show $ i + d.amount]) (i + d.amount)
 fnc (Tuple s i) (Withdraw w) =
 Tuple (s <> "\n" <> joinWith " " [show w.timestamp, "-" <> show w.amount, show $ i - w.amount]) (i - w.amount)
```

## Fire it up

Now we can write something like

```haskell
do
 deposit 500
 withdraw 100
 printStatement
```

which has type `StateT (Array Transaction) Effect Unit`. And we can run that computation with [`evalStateT`](https://pursuit.purescript.org/packages/purescript-transformers/4.2.0/docs/Control.Monad.State.Trans#v:evalStateT). Notice that the following code returns `Effect Unit`.

```haskell
flip evalStateT [] do
 deposit 500
 withdraw 100
 printStatement
```

## Show me the code

And here we have all the code

```haskell
data Transaction
  = Deposit Info
  | Withdraw Info

type Info =
  { timestamp :: DateTime
  , amount    :: Int
  }

deposit :: Int -> StateT (Array Transaction) Effect Unit
deposit amount = do
  ts <- lift nowDateTime
  let t = Deposit { timestamp: ts, amount: amount }
  modify_ \ts -> ts <> [t]

withdraw :: Int -> StateT (Array Transaction) Effect Unit
withdraw amount = do
  ts <- lift nowDateTime
  let t = Withdraw { timestamp: ts, amount: amount }
  modify_ \ts -> ts <> [t]

printStatement :: StateT (Array Transaction) Effect Unit
printStatement = do
  s <- gets toStatement
  lift $ log s

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
    deposit 500
    withdraw 100
    printStatement
```

## Outro

If you liked the post and want to help spread the word, please make some noise 🤘 But only if you really liked it. Otherwise, please feel free to comment or tweet me with any suggestions or feedback.

Thanks to [Liam Griffin](https://medium.com/u/98e431287677) who inspired me to try this exercise in PureScript with [his post in Haskell](https://medium.com/@Gryff/bank-kata-in-haskell-dealing-with-state-3364c13b994f).

Finally, I want to give a shoutout to [BusConf](https://www.bus-conf.org/) and to all the people I’ve met there that showed so much support for my PureScript journey. You are awesome!

If you are hungry for more, see how we can test the code in the followup: [Testing Bank Kata in PureScript](./2019-03-18-testing-bank-kata-in-purescript.html).
