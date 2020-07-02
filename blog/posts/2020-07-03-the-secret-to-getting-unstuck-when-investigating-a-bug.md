---
title: The Secret to Getting Unstuck when Investigating a Bug
description: "When Radek asked me to pair on investigating a bug I was skeptical. But I had something he lacked: ignorance."
published: false
author: Riccardo
cover_image: https://odone.io/images/investigator.png
tags:
  - Essential Skills
  - Team Work
---

> How does this code work?
>
> It rounds that number.
>
> What if that is a negative number?

When Radek asked me to pair on investigating a bug I was skeptical. My experience with Java and Android went back to whatever useless code I wrote at the university. In other words, I was (and still am) nowhere near his level. But I had something he lacked: ignorance.

Being stuck on a bug is a self-reinforcing process. We build a mental model of the code that is incorrect, we convince ourselves the problem is in a specific place and we limit the investigation to the wrong area. As [described last week](https://odone.io/posts/2020-06-26-why-good-solutions-block-better-ones.html), the Einstullung effect makes us ignore aspects that are not aligned with our mental model, looking more at what confirms it.

The best strategy to unstucking oneself is to take a step back. Unfortunately, it is incredibly hard. When debugging, the root cause seems to be five minutes away from being found. It's an infinite cycle of believing the next try is going to be the good one.

Sometimes it's not enough to just step away. Our mental model of the code is wrong and by using an incorrect map we would never get to the destination.

We need to talk to somebody else. This is even harder. We are so deep into the rabbit hole that the idea of explaining it all from scratch seems like a waste of time. And what if the other person, after listening to us, cannot give us the solution? Maybe we actually are just five minutes away from fixing it ourselves.

I believe that seeking the solution from somebody else is the wrong starting point. What we want to do is seeing the problem from scratch with their fresh eyes. Given this premise, pairing will never be a waste of time. If they happen to know a solution, great we are done. Otherwise, we updated our mental model to a less incorrect one.

The question is not how not to get stuck. We will get stuck, it's the nature of knowledge work. The question is what to do when it happens. My personal recipe is to timebox the amount of time I'm willing to be stuck by myself. When the timer is off I go look for help.

As highlighted above, we don't need to talk to the biggest expert in the problem domain. We need somebody with just enough experience to ask good questions. This is what will force us out of the bias of our mental model.

Back to our story. Radek had been stuck for several hours when we teamed up. Together we discussed what was the problem, found the source of the issue and devised a fix. All of that in under one hour. What I did was just asking a few questions, the crucial ones highlighted at the beginning of the post.
