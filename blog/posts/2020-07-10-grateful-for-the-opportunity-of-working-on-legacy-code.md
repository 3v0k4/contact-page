---
title: Grateful for the Opportunity of Working on Legacy Code
description: In the end it's my choice. I can be lazy, fixate on the small details and waste time. Or I can use some discipline, focus on the big picture and accept the challenge.
published: true
author: Riccardo
cover_image: https://odone.io/images/legacy.png
tags:
  - Essential Skills
---

> FIXME: This service doesn't handle charge errors from Stripe!
>
> FIXME: this is a temporary hack, should be replaced soon.
>
> FIXME: This is an abomination.

I know I was tasked to rescue a codebase. However, I was hoping for something better than that.

Earlier in the day, I received an email informing me I was granted access to the repository. Actually, two repositories: web-api and web-client. It's true the application has many features but users mostly interact with forms. The complexity of a frontend framework is not justified. I feel a rant starts growing inside me but I need to keep focus on the task, let's check the repos.

> FIXME: ...this is kinda useless, isn't it? Not sure why I put this here.

I already know the backend is written in Rails. Not the latest version but it's something I can live with. However, I'm shocked when I open the frontend repository. Not only it's an old release of Angular, also I find TypeScript, NGRX (Angular bindings for ReactiveX) and a ton of additional depencies. This is frustrating. I truly believe those are fine technologies when the complexity they introduce reduces the overall complexity of the project. But this is not the case. I really want to start screaming but my discipline puts me back on track. I need to install the application.

> FIXME: DHH says this should be in its own controller, and DHH is never wrong.

Hopeless, I look for a setup script. I find one in the backend repository. I cannot believe it. Unfortunately, it is the scaffolded one provided by Rails which was left pristine since day one. In the frontend no scripts at all. There are some useful information in the readmes. Miraculously, after a few minor bumps, I'm able to start the application. But it's not enough.

> FIXME: Is this actually doing something useful?

The database needs to be seeded with an admin user and some other things for the code to function. Unsurprisingly, the seed file provided by Rails is empty. But, as I've noticed in the docs, tests use some form of seeding. I decide to copy that code and move to the next step. Unfortunately, I notice that the code is not idempotent. Also, the email of the admin is random so developers need to check the database to know what email to use to login. Moreover, the second half of the file is commented out.

> FIXME: This is the wrong order details link, in the wrong email layout

Maybe I can copy the first part. Not really, I notice quickly that the seeds are creating data in third party services. This is not good. We will be needing to make sure the application works properly. Having all that noise is not an option. I reverse engineer the seeds and rewrite the code. After that, I cannot believe it, the application starts running. Now, I could start doing development. But let's be disciplined, I need to make sure I can rely on automated tests before I edit code. In the backend I fire the test suite and find out some tests are failing and 135 are skipped with comments like the following:

> FIXME: Not sure that these are necessary...
>
> FIXME: fix this

In general, there are a ton of tests. A ton too many. To balance that out, the frontend tests are non-existent.

## I take a step back.

I cannot help but think:

> What the hell was the original developer doing?

As soon as I catch myself saying that, alarm bells start ringing. If I give ground to those thoughts, I'll slip into a vicious cycle. Especially, in this situation where reminders are jumping at me. Line by line. Comment by comment.

I think ranting a bit in a private space is fine. It's like screaming in the pillow. But that should be it. Nobody gets to work thinking *cool, how can I screw up today?*. I'm sure **the original developer was doing their best crafting the code**.

Sure, mistakes were made. But I'm tasked with being part of the solution not part of the problem. Most of all, **legacy systems are an opportunity**. For something to be legacy means providing some sort of value. Otherwise it would be called trash.

This project is exciting. I get to work with a dream team of people from whom I'm learning a ton. Not to mention that, had it been written perfectly the first time, the product would have launched and I wouldn't have known it existed in the first place. In other words, this **legacy code enables me** to be here today.

No doubt I would have made different choices and we will need a plan on how to get out of this situation. Still, **it doesn't mean it cannot be fun** in the meanwhile. ReactiveX has been on my radar forever. Guess what, for the time being I'm stuck with it. I would not use a frontend framework in this case. But I'm glad I'm going to be learning a new one.

**In the end it's my choice**. I can be lazy, fixate on the small details and waste time. Or I can use some discipline, focus on the big picture and accept the challenge.

> FIXME: Clean up this logic; it's getting pretty messy.

Now, when I encounter a comment like this, I stop.

I'm grateful to be given this opportunity.
