---
title: How to Deal with (Your) Unsatisfying Code
description: In the past, I committed the crime of adding a "Refactor Feature X" ticket to the cemetery (aka backlog). Luckily, there are better ways.
published: true
author: Riccardo
cover_image: https://odone.io/images/shock.jpg
tags:
  - Essential Skills
---

Shit happens.

From time to time, I craft software that is not satisfying. Maybe it's messy, verbose, or namings are off. Maybe it's over-tested or under-tested. Maybe it's not correct, or maybe it's over-engineered. Maybe it just sucks, but I don't know why.

This happens when there's a gap between what I developed and what perfect could look like. It's also known as technical debt. The more of it, the more time it will take to understand the code in the future.

In the past, I committed the crime of adding a *Refactor Feature X* ticket to the cemetery (aka backlog). In other words, I handed off the problem to somebody else in the future (sometimes myself). It didn't work well, but, luckily, there are better ways.

## Technical Debt is Not All the Same

Let's say there's a box somewhere stuffed with spaghetti code. It works correctly and never requires changes. Should you rush to refactor it? Of course not; it's a debt that doesn't need to be repaid.

However, with bugs or spec changes, you have to open that box and understand what's going on.

If you had a choice, which one of the following parallel universes would you choose?

Parallel universe number one:

```ruby
class SomeClass
  # ...
  # 300 lines of code
  # ...
end
```

Parallel universe number two:

```ruby
class SomeClass
  def go_for_it
    data = FetchDataFromDatabase.go_for_it
    result = PerformSomeCalculations.go_for_it(data)
    SaveDataToDatabase.go_for_it(result)
  end
end

class FetchDataFromDatabase
  # ...
  # 100 lines of code
  # ...
end

class PerformSomeCalculations
  # ...
  # 100 lines of code
  # ...
end

class SaveDataToDatabase
  # ...
  # 100 lines of code
  # ...
end
```

I would pick number two.

The chaos that can be fit in a box is proportional to its volume: three smaller boxes are better than a big one. Most importantly, in the second universe, `SomeClass` works as an index. It gives a high-level overview of the algorithm. Also, it suggests what boxes to open to change a specific aspect of the feature. And there's a chance it's only one class, not all of them.

If you lost the keys to your car, would you rather know they are somewhere in the apartment or in one of the drawers close to the entrance?

## So What to Do with Unsatisfying Code?

If the specifications are stable and the code works, better to leave it alone.

If it's likely to change, try to make sure it works correctly, and it reveals intentions. In other words, on the one hand, make a change less likely, and, on the other hand, make sure the future reader will be guided throughout the code.

You can structure your process to (almost) guarantee code that is both correct and intention revealing. Should you accept the challenge, here's some food for thoughts:

- [Decomposing Features into Pipelines](https://odone.io/posts/2020-06-29-decomposing-features-into-pipelines.html)
- [How to Tame Complexity Into Simplicity with a Shake List](https://odone.io/posts/2020-08-28-how-to-tame-complexity-into-simplicity-with-a-shake-list.html)
- [Code Quality is Free (if You Do it Right)](https://odone.io/posts/2020-08-20-code-quality-is-free-if-you-do-it-right.html)
- [Naming Things Made Easy](https://odone.io/posts/2020-07-24-naming-things-made-easy.html)
- [Starting from the Problem not the Solution](https://odone.io/posts/2020-06-19-starting-from-the-problem-not-the-solution.html)
