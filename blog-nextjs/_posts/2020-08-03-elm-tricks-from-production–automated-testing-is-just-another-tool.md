---
title: Elm Tricks from Production–Automated Testing is Just Another Tool
description: Our take on unit testing and how we approached testing Elm code in AirCasting.
published: true
author: Riccardo
cover_image: https://odone.io/images/elm.jpg
canonical_url: https://blog.lunarlogic.io/2019/elm-tricks-from-production-automated-testing/
series: Elm Tricks from Production
tags:
  - Functional Programming
  - Elm
---

Since Lunar Logic is the oldest Ruby shop in Poland, it's no wonder people have been exposed extensively to Ruby and Rails. Being part of the community, we have gotten accustomed to its values. In particular, we have used automated testing a bunch. This is made easy by the gems and tools that are available in the ecosystem.

Unfortunately, when the friction of doing something approaches zero, so does employing it without questioning its costs and benefits. As a matter of fact, testing does not come for free: tests are code and as such require time to write and maintain.

We argue that automated testing is yet another tool to achieve a shorter feedback loop. But not the only one and not the right one in every possible situation. Also, tests can be used in a variety of scenarios: TDD, unit testing, property-based testing, acceptance testing, etc.

At the unit level, where we will focus our attention in this post, sometimes it's arguable if tests are needed. That is especially true when working with a language with a sound type system like Elm.

But let's talk with some code in front of our eyes. We will start with a stupid example just to prove the point and then move to production code from the open source [AirCasting](http://aircasting.org).

The first example is the `sum` function:

```elm
sum a b = a + b
```

Should we test it? Of course not! Let's forget for a second we shouldn't have written the function in the first place: there's no need to redefine `+`. In any case, the Elm compiler applies `+` only to `number`s. Also, the fact that `+` returns the correct result should be and is guaranteed by the language maintainers. Therefore, the only problems we could introduce in the `sum` function are using the wrong operator (e.g. `-`) or not using the addends properly (e.g. `sum a b = a`, `sum a b = a + 1`).

What about the following `boolToString` function?

```elm
boolToInt bool =
  case bool of
    True -> 1
    False -> 0
```

Elm enforces callers to use a Boolean for `bool`. That's because the function branches on `bool` with `True` and `False`. This function is so simple and declarative that we wouldn't put the effort to test. Where we could consider a test is when composing `sum` and `boolToInt` together in a more complex `sumBoolsOrDefault`:

```elm
sumBoolsOrDefault predicate bool1 bool2 default =
  if predicate
    then
      sum (boolToInt bool1) (boolToInt bool2)
    else
      default
```

Following a functional programming style the basic building blocks of a program are simple and declarative functions. Most of the times the developer and the type system are enough to guarantee the correctness. It's when composing simple things together that testing starts paying off. But where's the tipping point? It depends.

Let's take an example from [AirCasting](http://aircasting.org/fixed_map). In particular, each session at the bottom of the screen shows the timeframe in which measurements were taken. For recordings spanning multiple days we want to display "mm/dd/yyyy hh:mm - mm/dd/yyyy hh:mm" otherwise "mm/dd/yyyy hh:mm - hh:mm":

![Screenshot of AirCasting with the timeframe in a session card indicated](/images/aircasting_timeframe.png)

The logic resides in the [`Times`](https://github.com/HabitatMap/AirCasting/blob/320401a6fc83c57cd4436153e5744d6655a1e450/app/javascript/elm/src/Data/Times.elm) module. Let's follow the thinking process that brought us to the formatting code.

First of all, in AirCasting [`startTime` and `endTime`](https://github.com/HabitatMap/AirCasting/blob/3eb95a9bd3d711ec4b94d901de523f7bac0ac514/app/javascript/elm/src/Data/Session.elm#L22) for each session are [`Posix`](https://package.elm-lang.org/packages/elm/time/1.0.0/Time#Posix).

Therefore we first need to write functions to extract and format year, month, day, hour and minutes from `Posix` values.

For the year it's enough to use [`toYear : Zone -> Posix -> Int`](https://package.elm-lang.org/packages/elm/time/latest/Time#toYear) and take the last two digits:

```elm
toYear posix =
  posix
    |> Time.toYear Time.utc
    |> String.fromInt
    |> String.right 2
```

For the day we can use [`toDay : Zone -> Posix -> Int`](https://package.elm-lang.org/packages/elm/time/latest/Time#toDay) and pad a "0" to the left if needed to have two digits, thus:

```elm
toDay posix =
  posix
    |> Time.toDay Time.utc
    |> String.fromInt
    |> String.padLeft 2 '0'
```

For the month we can use [`toMonth : Zone -> Posix -> Month`](https://package.elm-lang.org/packages/elm/time/latest/Time#toMonth) and write a function to transform a `Month` into a string:

```elm
monthToString month =
  case month of
    Jan -> "01"
    Feb -> "02"
    Mar -> "03"
    ...
```

Hours and minutes follow a similar pattern:

```elm
toHour posix =
  posix
    |> Time.toHour Time.utc
    |> String.fromInt
    |> String.padLeft 2 '0'


toMinute posix =
  posix
    |> Time.toMinute Time.utc
    |> String.fromInt
    |> String.padLeft 2 '0'
```

Also, we need to distinguish between two posix values being on the same date or not:

```elm
areOnSameDate p1 p2 =
  toYear p1 == toYear p2 && toMonth p1 == toMonth p2 && toDay p1 == toDay p2
```

Should we test the functions mentioned above? In our case we haven't done so. In fact, they are all simple and declarative building blocks. Now, let's put everything together:

```elm
format start end =
  let
    toFullDate p =
      toMonth p ++ "/" ++ toDay p ++ "/" ++ toYear p ++ " " ++ toTime p
    in
      toFullDate start
        ++ (if areOnSameDate start end
          then
            "-" ++ toTime end
          else
            " - " ++ toFullDate end
          )
```

We used types to drive the development and the `format` function is where we decided to [test](https://github.com/HabitatMap/AirCasting/blob/320401a6fc83c57cd4436153e5744d6655a1e450/app/javascript/elm/tests/Data/TimesTests.elm). That's because the function is "complex" enough to necessitate test coverage.

What defines "complex" is hard to say. But I can steal some wisdom out of an [Elm discourse thread](https://discourse.elm-lang.org/t/what-not-to-unit-test/3511):

- it's hard to predict;
- tricky code;
- I fear or I know I will fear of changing some functions;
- encoders / decoders;
- sequence of actions for data;
- logic that can't easily be encoded in the type system;
- correctness in very important parts.

Be sure to read the thread mentioned above because it's full of great stuff. Thanks a lot to all the people who have participated and shared so much there!

Special thanks to [Rafał](https://blog.lunarlogic.io/author/rafal/) for proofreading this post.
