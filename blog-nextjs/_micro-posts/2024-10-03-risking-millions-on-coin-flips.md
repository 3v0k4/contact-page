---
title: Risking millions on coin flips
description: Using the Binomial Distribution to tilt the odds in your favor.
author: Riccardo
tags:
  - Statistics
---

I found this pearl in Naked Statistics (chapter 5):

> In 1981, the Joseph Schlitz Brewing Company spent $1.7 million for what appeared to be a shockingly bold and risky marketing campaign for its flagging brand, Schlitz. At halftime of the Super Bowl, in front of 100 million people around the world, the company broadcast a live taste test pitting Schlitz Beer against a key competitor, Michelob. Bolder yet, the company did not pick random beer drinkers to evaluate the two beers; it picked 100 Michelob drinkers.

It turns out, Schlitz's marketing department was not a bunch of idiots. And here's why.

If you toss a coin twice you can get either one of the following:
1. heads, heads
2. tails, tails
3. heads, tails
4. tails, heads

Therefore, the probability of getting one tails out of two flips is 2/4 (ie, case 3. and 4.).

That is described by the Binomial Distribution where n is the number of trails (coins) and k is the number of successes (tails):

```
p = n! / [k! * (n - k)!] * (1/2)^n
    2! / [1! * (2 - 1)!] * (1/2)^2
    2  / 1               *  1/4
    1/2 (same as 2/4 from above)
```

The statisticians at Schlitz considered that:
- commercial beers taste the same
- in a blind tasting, a Michelob drinker would say that Schlitz tastes better 50% of the times (it's a coin flip)
- getting 40% or more of Michelob drinkers to say Schlitz is better is worth $1.7 million

But what's the probability?

If you take 10 Michelob drinkers, you need at least 4 successes. In other words, you want to sum the probability of 4, 5, 6, 7, 8, 9, and 10 tails with coin flips:

```
p = 10! / [4! * (10 - 4)!] * (1/2)^10 +
    10! / [5! * (10 - 5)!] * (1/2)^10 +
    ...
    10! / [10! * (10 - 10)!] * (1/2)^10
  = 0,82
```

So there's a 82% chance of succeeding.

And if you raise the number of drinkers to 100, you get to a shocking 98%!
