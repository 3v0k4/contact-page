---
title: Why Good Solutions Block Better Ones
description: What is the Einstellung (set) effect, why it reduces problem solving skills and how to mitigate it
published: true
author: Riccardo
cover_image: https://odone.io/images/blindfolded.png
tags:
  - Essential Skills
  - Team Work
---

> Have you considered changing the one function being invoked instead of all the callers?

I could not believe I missed such a simple solution. But they were right, I could have changed one line instead of multiple files. One thing for sure, this ended up being one of the most influentials Pull Request I've been involved in.

I believe what shocked me was not being proven wrong. In fact, that's the exact reason why I appreciate code reviews. My surprise came from the fact that **I clearly employed a complex solution while the simple one was right in front of my eyes**. Still, I missed it.

I'm not particularly smart, thus I invest a lot into honing processes to be at the top of my game. This is why I could not accept the idea of being blindfolded. And who knows how many other times I made the same mistake.

I wanted to understand what happened to prevent it in the future. There's already enough complexity in software. **As a professional, I want to solve the essential complexity, not introduce accidental complexity**.

It took me quite some time to find an answer. I queried people all over the place. I was sure I was dealing with a bias but most folks mentioned the [Confirmation Bias](https://en.wikipedia.org/wiki/Confirmation_bias). However, its definition didn't satisfy me:

> Confirmation bias is the tendency to search for, interpret, favor, and recall information that confirms or supports one's prior personal beliefs or values

It took me several weeks, then I stumbled upon the following sentence:

> The Einstellung (set) effect occurs when the first idea that comes to mind, triggered by familiar features of a problem, prevents a better solution being found.

That. Was. It. I still remember reading the [paper](https://cognition.aau.at/download/Publikationen/Bilalic/Bilalic_etal_2008a.pdf) nodding the entire time.

Indeed what I had experienced was a bias. Unfortunately, as any other bias, it has its place. We probably do not want to be looking for the optimal solution at the supermarket for every item on the shopping list. However, there are situations where we need to **get it right, not just good enough**.

The number one trick I found to beat Einstellung is to **talk to somebody else**, a person with a different background and problem solving approach. If possible, it would be best not to bias them with our own solutions: individually solve the problem and only then discuss pros and cons. Not that it was needed, but here's another reason to support diversity.

> Ask yourself: why is this solution a bad one?

Trying to outsmart biases by oneselves is probably not a great strategy: they grow smarter as we do. But should the need arise here's a couple of other tricks to mitigate Einstellung: always devise multiple solutions and confront them. Most importantly, ask yourself "why is this solution a bad one?" then answer with as many reasons as possible.
