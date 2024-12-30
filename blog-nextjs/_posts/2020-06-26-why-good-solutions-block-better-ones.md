---
title: Why Good Solutions Block Better Ones
description: What is the Einstellung (set) effect, why it reduces problem solving skills and how to mitigate it
published: true
author: Riccardo
cover_image: /images/blindfolded.png
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

<blockquote class="pullquote"><span>Ask yourself: why is this solution a bad one?</span><a target="_blank" rel="noopener" href="https://twitter.com/intent/tweet?text=Ask%20yourself%3A%20why%20is%20this%20solution%20a%20bad%20one%3F%20via%20%40RiccardoOdone%0A%0A%23EssentialSkills%20%23TeamWork%0A%0Ahttps%3A%2F%2F{{TLD}}%2Fposts%2F2020-06-26-why-good-solutions-block-better-ones.html"><svg class="tweet-this-icon" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 400 400"><defs><style>.cls-1{fill:none;}.cls-2{fill:#f76ca5;}</style></defs><title>Twitter_Logo_Blue</title><rect class="cls-1" width="400" height="400"></rect><path class="cls-2" d="M153.62,301.59c94.34,0,145.94-78.16,145.94-145.94,0-2.22,0-4.43-.15-6.63A104.36,104.36,0,0,0,325,122.47a102.38,102.38,0,0,1-29.46,8.07,51.47,51.47,0,0,0,22.55-28.37,102.79,102.79,0,0,1-32.57,12.45,51.34,51.34,0,0,0-87.41,46.78A145.62,145.62,0,0,1,92.4,107.81a51.33,51.33,0,0,0,15.88,68.47A50.91,50.91,0,0,1,85,169.86c0,.21,0,.43,0,.65a51.31,51.31,0,0,0,41.15,50.28,51.21,51.21,0,0,1-23.16.88,51.35,51.35,0,0,0,47.92,35.62,102.92,102.92,0,0,1-63.7,22A104.41,104.41,0,0,1,75,278.55a145.21,145.21,0,0,0,78.62,23"></path></svg></a></blockquote>

Trying to outsmart biases by oneselves is probably not a great strategy: they grow smarter as we do. But should the need arise here's a couple of other tricks to mitigate Einstellung: always devise multiple solutions and confront them. Most importantly, ask yourself "why is this solution a bad one?" then answer with as many reasons as possible.
