---
title: From Temporary Knowledge to Permanent Knowledge
description: By disregarding practices and principles, we are condemned to start from scratch and re-learn at every context switch.
published: true
author: Riccardo
cover_image: /images/old-books.jpg
tags:
  - Essential Skills
series: Permanent Knowledge
---

My professional career started under the wings of two exceptional people: [Ania](https://twitter.com/szynszyliszys) and [Marcin](https://www.linkedin.com/in/marcinkostrzewa). They taught me something that stayed with me ever since.

I picture three elements of doing something: the WHY, the HOW, and the WHAT. We start with a goal, we rely on strategies and tactics, and execute.

During my internship, my mentors kept insisting on the WHY. I'm glad they entrusted me with figuring out the HOW and the WHAT. Because it's the WHY where I needed guidance.

They gifted me with an unfair advantage: extrapolating permanent knowledge from temporary knowledge.

## Temporary Knowledge

Temporary knowledge expires. There's a long list of frameworks and technologies that were popular just a few years ago and are already long forgotten.

Recently, I've read the following:

> Because of the temporary nature of the knowledge capital, computer programmers quickly reach a stage in their career when their old knowledge capital becomes worthless at the same rate as they acquire new knowledge capital. Their total knowledge capital is no longer increasing, so neither does their salary increase. They have reached the dead end plateau of their career, and it happens after less than ten years in the field.

To which somebody replied with:

> One difference I've seen from young developers and more mature developers is that more mature developers tend to have a better understanding of how users will operate and will help mitigate issues before they arise. They have wisdom and that certainly isn't "temporary knowledge capital".

As a developer, I wish the latter was true. However, I cannot help but think there's some merit to the first take. I posit both opinions are valid through the proper lens, and we better be aware of it. Let me explain.

The WHATs of anything are bound to its context and lifecycle. For example, the [syntax for records in Haskell](/posts/2020-06-01-records-haskell/) is relevant only to Haskell code. However, the HOWs and, to a more significant extent, the WHYs are part of the nature of software. They can be transferred to other languages.

Don't get me wrong, temporary knowledge is essential. It's a bit like shallow and deep work. Our jobs entail both. We need the WHATs, in other words, the concrete implementations, to perform a task. At the same time, we should be contemplating the HOWs and WHYs behind them.

One can write code without critical or creative thinking, but it doesn't mean they should do it. Copy-paste from StackOverflow, anybody?

The problem is, by disregarding practices and principles, we are condemned to start from scratch and re-learn at every context switch.

> I fear not the man who has practiced 10,000 kicks once, but I fear the man who has practiced one kick 10,000 times.
>
> –Bruce Lee

## Permanent Knowledge

Permanent knowledge does not expire because it's not bound to the current context. But it has a price. Given the higher level of generalization, it's often more challenging to acquire. It's a bit like in code, its easier to start with a concretion and, later, extract the abstraction.

Thus, one way to develop permanent knowledge would be laddering up from temporary knowledge. Instead of throwing arrays at problems, understand HOW to use them, most importantly, understand WHY. Syntax changes from language to language, but an array's nature is the same across the board. Hell, you could follow the abstraction to math, then it would apply to everything, not only software.

Recently, I started working on a [legacy project that is over-engineered with all sorts of technologies](/posts/2020-07-10-grateful-for-the-opportunity-of-working-on-legacy-code/). What saved me was transferring knowledge:

- Haskell -> RxJs
- Redux -> NgRx
- Elm -> TypeScript

It's not knowing Haskell, Redux, and Elm that enabled me to thrive in this project. It's the fact that I understand the principles behind those technologies. They are just an incarnation of category theory, reactive functional programming, and strongly-typed functional programming.

At the end of the day, we need a concrete implementation (temporary knowledge). But we cannot do a good job if we are not guided by permanent knowledge. I can easily search the details of an algorithm on a tree if I know it exists. If I don't, I'm going to re-invent the wheel, and possibly screw it up.

I feel I should close this post with a joke. Something about a JavaScript developer trying to convince a Ruby developer that the data structure they need is an object, not a hash. But I fear it would reflect the reality of the silos created by temporary knowledge around us.
