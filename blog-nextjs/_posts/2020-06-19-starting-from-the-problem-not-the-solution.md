---
title: Starting from the Problem not the Solution
description: I spent most of the day in solution space. The wrong solution space. In fact, I was solving a problem that didn't exist.
published: true
author: Riccardo
cover_image: https://odone.io/images/mac.png
tags:
  - Essential Skills
  - Team Work
---

**When I read "Sidekiq Frozen" I immediately start sweating.**

It's the subject of the email letting me know that Sidekiq, the technology that powers the project, is not working. Unfortunately, the client is from US, which means things have been stuck for several hours already.

**I need to fix it. Quickly.**

I immediately jump into the server and start reading the Sidekiq logs. I spot a bunch of errors and several warnings. Unfortunately, none of them help understanding what's the problem. Therefore, I start addressing them one by one. After a few, I realize I've burnt already a few hours.

**I'm nowhere done.**

But wait a second. What does it mean done? I don't have a definition of done: I've been tackling random errors and warnings. I don't even know what I'm supposed to be fixing: most of the features in the project rely on Sidekiq.

**I feel the stress building up.**

I decide to take a step back and check the Sidekiq logs for the second time. I notice that it is actually processing some stuff. In other words, it is not "frozen". I read the email again hoping I missed important clues on the first read but I cannot find anything relevant.

**Now, I'm desperate.**

The application is not so big so I start debugging it feature by feature, until I discover it's the data export that does not work. In particular, I don't receive the email containing the data I requested.

At this point I'm a couple of hours away from the daily call with the client. But experience taught me a couple of tricks to employ when I get stuck: run away from the desk and talk to somebody. I'm glad I follow that advice. In fact, a moment later, I'm at the coffee machine chatting with a colleague.

**Something fires in my head.**

Wait a minute, why am I seeking issues in Sidekiq? The problem is about an email not being sent. That's when I run to my computer and start looking at the mailing logs.

**Jackpot!**

Emails are being bounced because marked as spam. Phew, just in time to hop on the call and discuss with the client how to approach the problem.

## Outro

It's the end of the day. Looking back I feel like a champion: I found the issue in a haystack and coordinated the solution with the client. Perfect execution, huh?

Well, not really. Sure I have a good story for a blog post. Still, I could have found the issue within five minutes of reading the email. Instead, it took me an entire day.

> I made the mistake of starting from a solution and not from a problem
I made the mistake of starting from a solution and not from a problem. "Sidekiq Frozen" is a problem with a (wrong) solution embedded. Unfortunately, once I read that line bias kicked in and I started fixating on Sidekiq being the issue. Moreover, when I noticed some strange things in the Sidekiq logs I got even more confident I was on the right tracks.

I spent most of the day in solution space. The wrong solution space. In fact, I was solving a problem that didn't exist. Plus, without a definition of done, I didn't even know when to stop. This is not a particularly good combo.

This is a great reminder that understanding the problem is a prerequisite to finding the right solution, who would have guessed!
