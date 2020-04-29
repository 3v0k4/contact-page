---
title: On the Boyscout Rule
description: The Boyscout Rule by itself is a scam. Context is needed.
author: Riccardo
---

> Always leave the code better than you found it.

The good old Boyscout Rule. What a great punchline! It is something that I’ve heard saying and said myself so many times. Lately, I came to the realization that it’s a great way of dismissing the important discussions in a fancy way.

At the outset of a greenfield project, nobody would mention the Boyscout Rule. In fact, no code is the best code: there’s nothing to make better. However, entropy is a thing in software. One day, those perfect empty files will be filled with imperfect code. Until one day

> Please, since you are already working in the middle of this messy campground, consider leaving it better than you found it.
>
> –Somebody in a code review

There’s a huge problem though: what does it mean to "leave the code better than you found it"? There’s no absolute "better" in code. I know for sure because I often find myself in discussions on how to solve a specific problem. To me saying to somebody to do "better" without any definition of the term is like receiving an email from a client saying "the application does not work, please fix it".

Therefore, the Boyscout Rule by itself not only is not actionable but it can also be harmful. What if two developers have a different definition of "better" and are committed to "always leave the code better than you found it"? Well, you can imagine.

But hey, it’s waaay easier to tell somebody to follow the Boyscout Rule and move on with the next thing on the to-do list. The alternative is to discuss with people and figure out together as a team what to do. That. Is. Hard. Work. 

I’m not a boyscout myself but I hardly believe it goes something like this:

- Riccardo: Hey, I want to be a boyscout.
- Boyscouts: Cool, **always leave the campground better than you found it**.
- Riccardo: Huh?! That’s it?
- Boyscouts: Are you still here? Wait, were you expecting us to teach you the boyscout values and practices? Naaa, that’s hard work.

Of course it doesn’t work like that, it’s [way more sophisticated](https://beascout.scouting.org/)! Therefore, to invoke the Boyscout Rule more structure is needed.

First of all, goals need to be defined. We need to know where we are going to get there.

> There is no favorable wind for the sailor who doesn’t know where to go.
>
> –Seneca

That starts with the business talks. Customers use software because it provides value to them, not because the code is written in a specific way. Good code is at the service of that and not the other way around.

With the business context in mind, we can decide what would be the perfect code to support the goals. This is where the team should dream big but limited to [what makes sense for the project](https://odone.io/posts/2020-04-23-learning-commercial-projects.html).

So, we know where we are and where we want to be. It’s time to prepare a roadmap. Depending on the context it could be as generic as a list of tricks to improve the code to a more detailed timeline. At the minimum, the roadmap should explain what "better" means in the Boyscout Rule.

In some cases, things need to get worse before they can get better.

> You cannot make an omelet without breaking eggs.
>
> –Somebody over-communicating while cooking

Having different phases in a roadmap enables it. Always shooting for the local maximum means wasting the opportunity of reaching the global maximum via an intermediate suboptimal phase.

Another benefit of having a roadmap is being able to track things. Knowing where the team stands not only provides motivation but can also function as a reminder.

Lastly, improving code requires time and can be a thankless job. Be sure to do a lot of PR. On one hand, that reassures the business that spending 15% more time on each feature is a good investment. On the other, it puts the spotlight on those people working on the less-fun, more-legacy code.

I swear: I will never, ever invoke the Boyscout Rule again without context.
