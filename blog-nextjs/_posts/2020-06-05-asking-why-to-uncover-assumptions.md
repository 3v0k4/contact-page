---
title: Asking Why to Uncover Assumptions
description: Laddering up from solution space to problem space can save a ton of money (and timezones).
published: true
author: Riccardo
cover_image: https://odone.io/images/tickets.png
tags:
  - Essential Skills
  - Team Work
---

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

<blockquote class="pullquote"><span>We actually spent the next several hours using the developers' favourite key: backspace.</span><a target="_blank" rel="noopener" href="https://twitter.com/intent/tweet?text=We%20actually%20spent%20the%20next%20several%20hours%20using%20the%20developers%27%20favourite%20key%3A%20backspace.%20via%20%40RiccardoOdone%0A%0A%23EssentialSkills%20%23TeamWork%0A%0Ahttps%3A%2F%2Fodone.io%2Fposts%2F2020-06-05-asking-why-to-uncover-assumptions.html"><svg class="tweet-this-icon" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 400 400"><defs><style>.cls-1{fill:none;}.cls-2{fill:#f76ca5;}</style></defs><title>Twitter_Logo_Blue</title><rect class="cls-1" width="400" height="400"></rect><path class="cls-2" d="M153.62,301.59c94.34,0,145.94-78.16,145.94-145.94,0-2.22,0-4.43-.15-6.63A104.36,104.36,0,0,0,325,122.47a102.38,102.38,0,0,1-29.46,8.07,51.47,51.47,0,0,0,22.55-28.37,102.79,102.79,0,0,1-32.57,12.45,51.34,51.34,0,0,0-87.41,46.78A145.62,145.62,0,0,1,92.4,107.81a51.33,51.33,0,0,0,15.88,68.47A50.91,50.91,0,0,1,85,169.86c0,.21,0,.43,0,.65a51.31,51.31,0,0,0,41.15,50.28,51.21,51.21,0,0,1-23.16.88,51.35,51.35,0,0,0,47.92,35.62,102.92,102.92,0,0,1-63.7,22A104.41,104.41,0,0,1,75,278.55a145.21,145.21,0,0,0,78.62,23"></path></svg></a></blockquote>

Turns out, timezones were not *really really* needed. With that in mind, we went back and continued fixing the issues. No! Of course we didn't. We actually spent the next several hours using the developers' favourite key: backspace.

We made a mistake. We started with a solution (i.e. timezones). Asking why allowed us to ladder up to the problem. This is where we uncovered the assumption (i.e. users need to compare across timezones). Finally, we found a cheap way to validate it and discovered the problem was not there in the first place.

Unfortunately, we could not go back and avoid all the costs already incurred in development and maintenance. However, we saved money going forward. And we spared developers from having to deal with timezones in the future.

---

I stumbled upon ["Challenging requirements"](https://vimeo.com/31715562) while writing this post. Thanks [Gojko](https://gojko.net), not only it inspired me to change the structure of this article, you also gave me some great ideas for the future.
