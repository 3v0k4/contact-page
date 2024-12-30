---
title: Elm Tricks from Production–From Angular v1 to Elm in 4 Days
description: A solid plan and some patience is all you need to migrate an entire application to a different technology (while it continues to run).
published: true
author: Riccardo
cover_image: /images/elm.jpg
canonical_url: https://blog.lunarlogic.io/2020/elm-tricks-from-production-angular-to-elm/
series: Elm Tricks from Production
tags:
  - Functional Programming
  - Elm
---

You might be wondering, how the hell did they manage to migrate the entire frontend of AirCasting from Angular to Elm in 4 days? The answer is simple: we cheated. But we swear this is totally legit and we encourage people to do the same!

In a [previous post](https://blog.lunarlogic.io/2019/elm-tricks-from-production-migration/) we explained why and how we choose not to rewrite the entire frontend from scratch:

> The best way to rewrite any application from one technology to another is incrementally, while it continues to run. At first the new language can take over separate parts of the application and then gradually incorporate more and more code.

In particular, over the last year, we have been using new features as an excuse to bring us closer to the finish line. In other words, we never stopped producing value for our customers. Also, having the removal of Angular in the back of our minds at all times, allowed us to come up with more and more tricks on how to make it happen. We actually kept a list on the wall in front of us to function as a constant reminder.

After several months of work, we realized we got close enough to remove Angular completely. That is when we took 4 days off the development of new features to [complete our mission](https://github.com/HabitatMap/AirCasting/pull/388).

We are pretty sure the cost of those 4 days was worth it. In fact, it will probably be repaid in the next iteration not needing to learn Angular v1 or having to deal with its complexity.

There is still a long way in front of us. As a matter of fact, we only removed Angular by extracting code to vanilla JavaScript and kept the Elm ports. But, for sure, the future looks brighter now!
