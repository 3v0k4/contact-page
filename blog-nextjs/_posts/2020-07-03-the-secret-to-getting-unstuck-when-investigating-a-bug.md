---
title: The Secret to Getting Unstuck when Investigating a Bug
description: "When Radek asked me to pair on investigating a bug I was skeptical. But I had something he lacked: ignorance."
published: true
author: Riccardo
cover_image: /images/investigator.png
tags:
  - Essential Skills
  - Team Work
---

> How does this code work?
>
> It rounds that number.
>
> What if that is a negative number?

When Radek asked me to pair on investigating a bug I was skeptical. My experience with Java and Android went back to whatever useless code I wrote at the university.

I was (and still am) nowhere near his level. But I had something he lacked: ignorance.

## Being stuck on a bug is a self-reinforcing process

We build a mental model of the code that is incorrect, we convince ourselves the problem is in a specific place, and we limit the investigation to the wrong area.

As [described last week](/posts/2020-06-26-why-good-solutions-block-better-ones/), the Einstullung effect makes us ignore aspects that are not aligned with our mental model, looking more at what confirms it.

The best strategy to unstucking oneself is to take a step back. Unfortunately, it is incredibly hard. When debugging, the root cause seems to be five minutes away from being found. It's an infinite cycle of believing the next try is the good one.

Sometimes it's not enough to just step away. Our mental model of the code is wrong and by using an incorrect map we can't get to the destination.

## We need to talk to somebody else

This is even harder. We are so deep into the rabbit hole that the idea of explaining it all from scratch seems like a waste of time. And what if the other person, after listening to us, can't give us the solution?

The fact is that seeking a solution from somebody else is the wrong starting point. Instead, we want to see the problem from scratch with their fresh eyes, so pairing will never be a waste of time.

Do they have a solution? Great, we are done. No solution? Congrats, we just updated our mental model to a less incorrect one.

The question is not how not to get stuck. We will get stuck, it's in the nature of knowledge work. The question is what to do when it happens. And my personal recipe is to timebox the amount of time I'm willing to be stuck alone before I seek help.

As highlighted above, we don't need to talk to the biggest expert, we need somebody with just enough experience to ask good questions. This is what will force us out of the bias of our mental model.

## Back to our story

Radek had been stuck for several hours when we teamed up. We discussed together what was the problem, found the source of the issue and devised a fix. All of that in under one hour.

What I did was just asking a few questions.

---

To this day, I still remember that pairing session with you and the many others together with joy. The cross-platform, cross-technology, cross-language sessions have been something really special. Thank you, Radek.
