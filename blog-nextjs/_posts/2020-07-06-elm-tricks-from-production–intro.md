---
title: Elm Tricks from Production–Intro
description: How Elm enabled fast iterations, cutting costs and keeping everybody happy in one of our projects at work—AirCasting
published: true
author: Riccardo
cover_image: /images/elm.jpg
canonical_url: https://blog.lunarlogic.io/2019/elm-tricks-from-production-intro/
series: Elm Tricks from Production
tags:
  - Functional Programming
  - Elm
---

We are happy to announce the release of a new version of [AirCasting](http://aircasting.org)!

This is exciting for us because the application not only has a new look and many new features. There's also been a lot of work on the infrastructure, performance and technology.

Being a seven-year-old product, it had accumulated some rust. In fact, AirCasting has been using the first version of Angular since inception. Unfortunately, using an outdated web framework from 2010 makes work cumbersome and slow. That translates into higher costs, many bugs and a lot of frustration.

Gladly, this last iteration was about [revamping the entire user interface](https://dribbble.com/shots/6790675-AirCasting-environmental-data-monitoring-app): the perfect excuse to introduce a new technology, cut costs and improve the product.

At Lunar Logic we have our group of functional programming aficionados: we have used Elm, ["a delightful language for reliable webapps"](https://elm-lang.org/), in the past and decided to do the same for AirCasting. Our experience has confirmed once again how great of a technology Elm is. In particular, in the last few months of use we have noticed:

- A dramatic decrease of bugs: the Elm type system prevents invalid states and catches a lot of problems in development, way before the code gets to production.

- Changing code is like a walk in the park: in Elm we say "if it compiles, it works". That means once the program is written and it works as expected, it's really easy to modify or add new features.

- It's a joy to work with: developers have a great time working with Elm, users enjoy error-free applications and stakeholders can build features fast and effortlessly.

In other words, Elm enables fast iterations, cutting costs and keeping everybody happy. But don’t take our word for it, this is a testimonial by Michael Heimbinder, Founder & Executive Director of HabitatMap (AirCasting):

> Moving AirCasting from Angular to Elm has simplified the process of developing new features and identifying and zapping bugs. The comparative ease of working in Elm makes for better communication between members of the development team, which results in better code in less time

The Elm community is not as mainstream as React's and Angular's. Therefore, the ecosystem is smaller and fewer developers are up-to-speed with the technology. We argue that it doesn't really matter how big an ecosystem is, as long as it's big enough. When it comes to programmers, we expect to attract the people who became dissatisfied with traditional approaches and challenged the status quo. [At NoRedInk that is already reality](https://youtu.be/5CYeZ2kEiOI?t=1447):

> Elm is the #1 reason developers apply

Since AirCasting is powered mostly by Elm now, we have decided to publish an "Elm Tricks from Production" blog series. So fasten your seatbelt and get ready to see:

- why Elm is the best;
- why AirCasting is cool;
- real Elm code from production;
- what concepts Elm entails that could be applied in other languages / frameworks.
