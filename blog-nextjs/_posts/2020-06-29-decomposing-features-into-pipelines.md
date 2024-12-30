---
title: Decomposing Features into Pipelines
description: Code and pray when devising a solution is not the best approach. The exploration needs to start in problem space.
published: true
author: Riccardo
cover_image: /images/pipeline.jpg
tags:
  - Functional Programming
---

What is a feature?

Sometime it's as an impure action:

```bash
--> do_something -->
```

Sometimes it's a pure calculation:

```bash
input --> output
```

Most of the times it's a mix of the two:

```bash
input --> do_something --> output
```

In other words, a feature is just an algorithm: data in, action, data out. With that in mind, we can go back to that scary huge feature and model it that way:

```bash
a_ton_of_input --> do_a_ton_of_stuff --> a_ton_of_output
```

Done, profit!

Well, not really. It's not that simple.

`a_ton_of_input` could be complicated, including nested structures, optional data and varying schema. `do_a_ton_of_stuff` could entail network requests, integrations with other processes and complex calculations. `a_ton_of_output` could require a strange shape, different formats depending on the requests and conditionals all over the place. And that is just the happy path!

**Breaking down a problem is a fundamental tool in the developer's toolbox**. We need to be able to go from:

```bash
input --> do_something --> output
```

To a pipeline like:

```bash
input --> do_something --> do_something --> do_something --> output
```

But hey, now we have four problems: three behaviours plus the recomposition. Yes! Four *much simpler* problems than what we started with. As soon as a problem is broken down, the **actions and calculations should be straightforward** to implement. And the **composition doesn't have to be more complicated than piping in Bash** (i.e. `|`).

The nice part is that we do not need a fuctional language to model a problem this way:

```python
data_1 = do_something_1(input)
data_2 = do_something_2(data_1)
output = do_something_3(data_2)
```

Surprisingly, **the hard part is breaking down the problem into subproblems**. If anything, as developers, we often make the mistake of immediately pulling a ticket, writing code and refactoring. This is not breaking down a problem. This is coding a solution and then praying refactoring will make it nice and correct.

**Finding subproblems means exploring the problem space**. Not contaminating our minds with any specific solutions will keep us creative. Maybe to solve the problem there's no need to write any code at all? Furthermore, refactoring on a whiteboard is pure joy. Especially, when it turns out that jumping into code without a thought made us solve the problem in the wrong way.

> Flowchart until you think you understand the problem. Write code until you realize that you don't.
>
> –[NATO science committee (1968)](http://homepages.cs.ncl.ac.uk/brian.randell/NATO/nato1968.PDF)

I got a challenge for us all. For the next few tickets we pull, let's setup a timer and spend some time in problem space instead of starting from the solution. Should you embark on the journey, please share your experience.
