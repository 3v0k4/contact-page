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

<blockquote class="pullquote"><span>I made the mistake of starting from a solution and not from a problem</span><a target="_blank" rel="noopener" href="https://twitter.com/intent/tweet?text=I%20made%20the%20mistake%20of%20starting%20from%20a%20solution%20and%20not%20from%20a%20problem%20via%20%40RiccardoOdone%0A%0A%23EssentialSkills%20%23TeamWork%0A%0Ahttps%3A%2F%2Fodone.io%2Fposts%2F2020-06-19-starting-from-the-problem-not-the-solution.html"><svg class="tweet-this-icon" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 400 400"><defs><style>.cls-1{fill:none;}.cls-2{fill:#f76ca5;}</style></defs><title>Twitter_Logo_Blue</title><rect class="cls-1" width="400" height="400"></rect><path class="cls-2" d="M153.62,301.59c94.34,0,145.94-78.16,145.94-145.94,0-2.22,0-4.43-.15-6.63A104.36,104.36,0,0,0,325,122.47a102.38,102.38,0,0,1-29.46,8.07,51.47,51.47,0,0,0,22.55-28.37,102.79,102.79,0,0,1-32.57,12.45,51.34,51.34,0,0,0-87.41,46.78A145.62,145.62,0,0,1,92.4,107.81a51.33,51.33,0,0,0,15.88,68.47A50.91,50.91,0,0,1,85,169.86c0,.21,0,.43,0,.65a51.31,51.31,0,0,0,41.15,50.28,51.21,51.21,0,0,1-23.16.88,51.35,51.35,0,0,0,47.92,35.62,102.92,102.92,0,0,1-63.7,22A104.41,104.41,0,0,1,75,278.55a145.21,145.21,0,0,0,78.62,23"></path></svg></a></blockquote>

I made the mistake of starting from a solution and not from a problem. "Sidekiq Frozen" is a problem with a (wrong) solution embedded. Unfortunately, once I read that line bias kicked in and I started fixating on Sidekiq being the issue. Moreover, when I noticed some strange things in the Sidekiq logs I got even more confident I was on the right tracks.

I spent most of the day in solution space. The wrong solution space. In fact, I was solving a problem that didn't exist. Plus, without a definition of done, I didn't even know when to stop. This is not a particularly good combo.

This is a great reminder that understanding the problem is a prerequisite to finding the right solution, who would have guessed!
