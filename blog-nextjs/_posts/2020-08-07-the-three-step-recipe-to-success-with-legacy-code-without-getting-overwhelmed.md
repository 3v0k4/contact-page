---
title: The Three-Step Recipe to Success with Legacy Code without Getting Overwhelmed
description: The secret to successfully surviving legacy code is to build momentum. We need to focus on the solution not on the problems. In other words, we need to keep in mind that our goal is a successful project not perfect code.
published: true
author: Riccardo
cover_image: /images/legacy.jpg
tags:
  - Essential Skills
---

It's the first day of work in the project. I'm fired up. The task is simple: update the label of a button. **But the smile on my face doesn't last for too long**.

I quickly notice the readme does not cover all the installation steps. Then, it turns out the database needs to be seeded but no documentation to be found about that either. Then, I discover there's a ton of tests. A ton too many. Including dozens that are skipped and several are red straight from the main branch. Then, I find out the list of technologies employed in the project is long. Too long for a crud application. Then, I realize they are all outdated. Then, my eyes land on the 546 uses of `any` in the codebase. Then, I see the compiler is not using strict mode which is like saying "[Billion dollar mistake](https://en.wikipedia.org/wiki/Tony_Hoare)? BRING IT ON!". Then, I notice a comment in the code ([among others](/posts/2020-07-10-grateful-for-the-opportunity-of-working-on-legacy-code/)):

> FIXME: This is an abomination.

Then, the cherry on top, the application is not one but two repositories: frontend and backend. That would make my pain double, unless there were some news in the other repo. In fact, there are but not what I wished for: no tests at all.

## When starting on a legacy project, it's impossible to do all the right things

There is no way we can address the hacks, deploy the best practices we know and develop a comprehensive mental model of a codebase that was written in a few years by multiple developers. We could try but it would only make the competition happy. Thing is, systems are not built messy. They grow complex over time. **We cannot expect to undo years of work in a few weeks**. That shit takes time. In AirCasting it took us [two years to migrate the frontend](/posts/2020-07-06-elm-tricks-from-production–intro/). And it's still on-going but at least we never stopped to fully re-write anything.

 Once I asked a doctor how they cope with a job where they are exposed to bad stuff all day. He told me

> I don't focus on the problems, I focus on being part of the solution.

In my experience, **the secret to successfully surviving legacy code is to build momentum**. We need to focus on the solution not on the problems. In other words, we need to keep in mind that our goal is a successful project not perfect code. In a legacy codebase we need to keep our eyes on the objective. If we allowed ourselves to take stops on the way to examine unrelated problems, we would get lost. It's way too many.

So, let's **start simple**. Remember, the goal is not best code ever. Instead, we should aim for good enough. That means being ready to challenge everything, including "best" practices. Maybe we can skip some testing here, hack it a bit over there and do some edit and pray in a couple of other places. Let's **embrace the discomfort**. If this application survived years in that messy state, it can surely survive some more time while we get comfortable with it.

**Momentum is one of the most motivating things**. Who doesn't like the breeze of tickets flowing on the Kanban board? Unfortunately, we are still not done. Once we ace our simple process and things start moving forward, it's time to shift gears. Now, that we are on the move equipped with a better mental model of the codebase, what's the one thing we can **add to the process to fuel the most momentum**? Remember, we want to have a job, so the objective is in the business not in code.

## I take a deep breath

The reason why I need to change that label is to allow the team to run user interviews. We need that momentum. Thus, I decide to start simple, I edit and pray. As a matter of fact, I don't even install the app on my machine. I just edit, push and start looking at each step of the CI pipeline. They start turning green one after the other. I keep praying. Several painful minutes later it gets to a step named "deploy". At this point I'm so tensed I'm trying to help CI by visualizing the circle becoming green. And it does, it's green. We made progress! Now, I know I can do it, I'm [ready for the challenge](/posts/2020-07-10-grateful-for-the-opportunity-of-working-on-legacy-code/).
