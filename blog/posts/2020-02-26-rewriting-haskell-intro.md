---
title: Rewriting to Haskell–Intro
description: Hidden in the Functional Programming basement at Lunar Logic we want to have fun, enable newcomers to approach Haskell and creating future options for the company
author: Riccardo
series: Rewriting to Haskell
tags:
  - Functional Programming
  - Haskell
  - Servant
---

At Lunar Logic we use an [internal web application](https://lunarlogic.io/design/stream) to handle announcements. Stream has been used by the entire company for the last 6 years or so. Over time we have discovered it's a great place to run tech experiments: production enough to feel real but internal enough not to cause too much damage if stuff goes wrong.

Stream was born as a Rails application, then transitioned to Rails API + Ember and after that Rails API + Elm. Rewriting the frontend to a functional language got some of us hooked into Functional Programming. As a matter of fact, on Slack somebody said "We've rewritten the frontend 100%. Do we really want to not have types in the backend too? 😏". This seed has been growing for a couple of years, until today we decided to take action.

We gathered the FP aficionados in Lunar and decided to rewrite the backend to Haskell. In particular, we want to focus on the following goals:

- no committments, we will go on as long as it's fun;
- making Haskell accessible to newcomers;
- creating a proof of concepts for future projects;
- small valuable iterations like we did for [AirCasting](https://blog.lunarlogic.io/2019/elm-tricks-from-production-migration/).

I've personally worked in Ruby and JavaScript for a long time and the only [real contribution](https://odone.io/posts/2019-08-26-building-a-blog-in-haskell-with-yesod–giving-back.html) I made to OSS was to Yesod 😅
We truly believe Functional Programming is a great tool and perfect place to give back, thus together with [Alex](https://www.linkedin.com/in/alexander-suminski/), my partner in crime for this project, we want to spread the joy. Therefore, we decided to share with the world our journey rewriting Stream to Haskell.

Stay tuned!
