---
title: Asking Why to Uncover Assumptions
description: Laddering up from solution space to problem space can save a ton of money (and timezones).
published: true
author: Riccardo
tags:
  - Essential Skills
  - Team Work
---

![Handmade sketch displaying 3 stacked tickets each with a bug and the word timezone on the left and an astonished face on the right](/images/tickets.png)

Stop. Consider the task you are doing.

**Why are you working on it? Why is it important? Why?**

Recently, I worked in a project that enabled users to record measurements. We decided to prioritize bugs and rapidly noticed a ton of issues related to timezones. Who would have guessed?!

We jumped in and started moving tickets from to-do to done. We were working fast but there were so many we could never see the finish line. This is when we stopped and asked why.

> Why are timezones important?
> Because we have them in the product.
>
> But why do they really matter?
> Because we are recording the local time of each measurement.
>
> But why do users really really want to have timezones?
> Because they compare measurements taken across timezones.
>
> But why do they do that?
> Because.. Because.. Well, that's a good question!

```pullquote
We actually spent the next several hours using the developers' favourite key: backspace.
```
Turns out, timezones were not *really really* needed. With that in mind, we went back and continued fixing the issues. No! Of course we didn't. We actually spent the next several hours using the developers' favourite key: backspace.

We made a mistake. We started with a solution (i.e. timezones). Asking why allowed us to ladder up to the problem. This is where we uncovered the assumption (i.e. users need to compare across timezones). Finally, we found a cheap way to validate it and discovered the problem was not there in the first place.

Unfortunately, we could not go back and avoid all the costs already incurred in development and maintenance. However, we saved money going forward. And we spared developers from having to deal with timezones in the future.

---

I stumbled upon ["Challenging requirements"](https://vimeo.com/31715562) while writing this post. Thanks [Gojko](https://gojko.net), not only it inspired me to change the structure of this article, you also gave me some great ideas for the future.
