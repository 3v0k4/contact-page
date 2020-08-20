---
title: Code Quality is Free (if You Do it Right)
description: "The secret is simple: the first version of the code must be as close to perfection as possible. Here's how to do it."
published: true
author: Riccardo
cover_image: https://odone.io/images/hill.jpg
tags:
  - Essential Skills
---

*It's been two weeks of hard work, but my code works great in production. Before merging I take a look at the pull request: wow, it looks awful! I'm not even sure I can refactor my way out of this spaghetti mess. And even then, it would be several days of work.*

It's similar situations that make people think stuff like:

> Quality costs, that's why we cannot afford it

That sounds like nonsense to me. Let me explain.

If we approximate code quality to correctness plus maintainability, the software we write will land in between two imaginary lines: perfect and trash.

```bash
–––– Perfect


xxxx Code

–––– Trash
```

Sure, refactoring lifts code upwards. But the closer to trash the more refactoring is needed to reach an acceptable threshold (perfection is unachievable).

## Work has two phases: figuring things out and making it happen

Basecamp does an awesome job at [visualizing it](https://basecamp.com/features/hill-charts). Similarly to drafting and editing, mixing the two is the perfect way to do a sloppy job.

Imagine somebody preparing a poached egg with no prior knowledge. Would throwing eggs in the pot be an efficient way to figure it out? Of course not, they could get over the hill by watching a five-minute video on YouTube. For the same reason, we do not want to start a task by throwing code at the problem.

**The secret is simple**: the first version of the code must be as close to perfection as possible. If this feels too daunting, think landing as far as possible from the trash line. In fact, we just want to achieve correct code that needs the least rework possible. Avoiding silly mistakes (that would need refactoring later) early in the process is a huge win.

There are multiple ways of **figuring things out**. Probably some work better than others depending on the context. But the one I default to is whiteboarding. If you like quick refactoring and your favorite key is delete, you are in for a treat when it comes to working with an erasable marker. But we need a definition of done, otherwise, it would take us forever to get close enough to perfection. The way I know I'm ready for the execution phase is when the [feature is decomposed in a pipeline](https://odone.io/posts/2020-06-29-decomposing-features-into-pipelines.html) that is straightforward to implement.

Now it's time to **making it happen**. Translating the steps of the pipeline in code is another hard skill. But, at least the figuring out part is behind us and we can focus on writing the best software. However, we can get even more quality for free by raising the default level of code that gets out of our fingers. There are no shortcuts, the best way is to read a lot of code and write a lot of code, mindfully.

## Sometimes I write code I don't like

Partly is because I'm raising the bar of perfection besides making my default code better. Some other times, I just screw it up. But I learn from my mistakes because **quality tastes better when it's free**.
