---
title: 10 Knowledge Transfers that Make me Thrive with Legacy Code
description: I don't have to relearn anything from scratch; I only transfer knowledge over from past experiences and fill the blanks.
published: true
author: Riccardo
cover_image: https://odone.io/images/camping.jpg
tags:
  - Essential Skills
series: Permanent Knowledge
---

It's a sunny day, like many others. You just got to the trash room to throw away the rubbish. Something strange jumps to your eyes as soon as you open one of the bins. It's a metal box. Lifting it up, you notice it ticks the seconds, and it displays a countdown on a red LED screen. Currently, it's marking 01:14 and going down. A jolt of adrenaline starts circulating in your blood: **it's a bomb**.

You are in luck; it's the perfect day to find explosives. Yesterday, you spent the entire evening on a Mission Impossible marathon. You manage to calm down and recall what needs to be done: **cut the blue wire**. You lift the box with a smooth movement, lay it down, and open the lid with care.

Cut the blue wire. Cut the blue wire. Cut the blue wire. To your dismay, as soon as the circuit is exposed, you notice wires of all colors but blue.

**Legacy software is that bomb**. Every time, it's different in its own dramatic way: there are the challenges of greenfield code and all the constraints coming from choices made in the past. As they say, it's easy to perform well when things are predictable and straightforward. Unfortunately, you don’t know what to expect when dealing with legacy code until you open the ticking box.

I'm [currently rescuing such a codebase](https://odone.io/posts/2020-07-10-grateful-for-the-opportunity-of-working-on-legacy-code.html). It's not so awful because I've seen more spaghetti in my life. Trust me, I'm Italian. However, it's problematic enough to have discouraged developers in the past.

After several weeks, I'm proud to say that not only I overcame the mess, but also I've been thriving in this project. I credit investing in transforming [temporary knowledge into permanent knowledge](https://odone.io/posts/2020-10-01-from-temporary-knowledge-to-permanent-knowledge.html). In other words, I didn't have to relearn anything from scratch; I only transferred knowledge over from past experiences and filled the blanks.

Here are the ten knowledge transfers that saved my ass:

## 1. Redux

In the past, I've dealt with several React applications that made use of Redux to manage state. But I didn't stop there. I decided to [learn Redux's principles](https://medium.com/hackernoon/selectors-in-redux-are-a-must-d6b0637c79b7) instead of just learning how to use it in a specific context. In other words, I laddered up from API bindings to architecture.

The project I'm working on employs NgRx, which, in turn, relies on Redux. I didn't have to start over with reducers, selectors, and so on. I took a quick glance at the NgRx's docs, and I was good to go.

## 2. Confidence

Legacy code is an emotional rollercoaster. Every up is matched by a down. Some days you think you've killed it just to realize later you screwed it up.

In the past, I had several experiences like that. This is why I'm confident that, as long as there is a roadmap, things will turn out ok; it's just a matter of time. Hell, it [took us two years to remove Angularjs from AirCasting](https://odone.io/posts/2020-08-10-elm-tricks-from-production–from-angular-v1-to-elm-in-4-days.html).

Another superpower I embrace is being at ease with ignorance. There is no way to know each and every corner of a codebase. It's quite dangerous to delude oneself of the contrary. I'm confident in being ignorant because it [actually makes a difference](https://odone.io/posts/2020-07-03-the-secret-to-getting-unstuck-when-investigating-a-bug.html).

## 3. Refactoring and Code Design

The constraints of a legacy codebase require expertise and flexibility. The strategies and tactics you apply by the book in a greenfield situation won't work. And thinking otherwise will result in [being overwhelmed](https://odone.io/posts/2020-08-07-the-three-step-recipe-to-success-with-legacy-code-without-getting-overwhelmed.html).

Most of the time, clean red, green, refactor cycles won't be an option. Also, you won't get to the perfect architecture on the first attempt. Legacy code grows messy over a long period, we should not expect to fix it in one take.

Nowadays, I learned to obtain feedback, not necessarily via TDD, to refactor with confidence. At the same time, I developed an eye for design (object-oriented and functional). Without it, the only refactoring one can afford is small in scope, like renaming a method or extracting a function.

I believe that [code quality is free when done right](https://odone.io/posts/2020-08-20-code-quality-is-free-if-you-do-it-right.html), and in [coding simple solutions to complex problems by design](https://odone.io/posts/2020-08-28-how-to-tame-complexity-into-simplicity-with-a-shake-list.html).

## 4. Being a Team Player

I'm astonished by the miracle that a team worked when I look at delightful software. There are so many things that could go wrong. The most challenging part is the coordination, though. Designers and developers need to understand each other. The team needs to foster feedback from the customers and analyze all the nuances. Everybody needs to agree on tabs vs. spaces.

I'm a fan of [creating a team playbook](https://odone.io/posts/2020-08-14-how-to-conjure-your-team-magic-with-a-few-stickies-and-the-playbook-exercise.html), writing [livable code](https://odone.io/posts/2020-05-15-living-together-team.html), and focusing on [generativity (rather than productivity)](https://odone.io/posts/2020-05-08-on-productivity.html).

## 5. Having a Product Mindset

A delightful product is not born by [just moving tickets to done](https://odone.io/posts/2020-09-10-99-percent-done.html), [deploying the latest bleeding-edge technology](https://odone.io/posts/2020-04-23-learning-commercial-projects.html), or [solving the wrong problems](https://odone.io/posts/2020-06-19-starting-from-the-problem-not-the-solution.html).

Everything is a hypothesis until it's validated. We need to [uncover assumptions](https://odone.io/posts/2020-06-05-asking-why-to-uncover-assumptions.html), [ask silly questions](https://odone.io/posts/2020-05-29-silly-questions.html), and [find and treasure the most controversial disagreements](https://odone.io/posts/2020-06-12-measuring-disagreement-with-standard-deviation.html).

## 6. Angular v4

This project could have been built in Rails. Unfortunately, it was overengineered. Every time I'm changing a form, I cannot help but think I'm doing double the work, with double the risks, without any gain.

My only experience with Angular has been with [Angularjs in AirCasting](https://odone.io/posts/2020-07-06-elm-tricks-from-production–intro.html). However, combined with several other projects on the frontend, it jumpstarted my understanding of Angular v4. Two-way data bindings, components' lifecycle, automatic dependency injection, routing, among other things, are common concepts across the board.

## 7. RxJs

Angular v4 makes extensive use of RxJs. Here, I found myself at home, thanks to my experience with Haskell and PureScript. As soon as I understood I was dealing with composition, functors, monads, and the like, I just did the functional programming thing: [decomposed to simple problems and recomposed with the correct operators](https://odone.io/posts/2020-06-29-decomposing-features-into-pipelines.html).

## 8. TypeScript

In Angular v4, types are a thing. To be honest, it was a let down to discover that, by default, `any` and `null` are allowed in TypeScript. The project I'm working on has 546 occurrences of `any`s, and makes extensive use of `null`s.

It's a frustrating experience coming from Elm where runtime exceptions don't exist. Not all is lost, though; many of the techniques I learned in Elm to [make programs correct by design](https://odone.io/posts/2020-07-20-elm-tricks-from-production–declarative-bug-free-user-interfaces-with-custom-types.html) can still be employed.

## 9. Tweaking Performance

Rescuing the codebase, we realized it's slow and breaks when there is too much data to send to the frontend. Who would have guessed pagination in a CRUD application was a requirement?!

Performance work is not my best suit, but permanent knowledge helped. I haven't stopped at optimizing Ruby and Rails code in the past. I learned to [make things reproducible](https://odone.io/posts/2020-09-16-how-to-investigate-performance-issues-in-a-web-app-with-a-simple-script.html), measure them, and [tweak the most significant bottlenecks first](https://odone.io/posts/2020-09-23-making-an-endpoint-13-times-faster.html).

## 10. Solving Problems

As knowledge workers, we should go all-in to cultivate the ability to perform cognitively demanding work without interruptions. In a disrupted world, it's becoming rarer and rarer to find people who can sustain the discomfort of long and deep sessions of work.

Just yesterday, I opened a file, looked at it from multiple perspectives, and stared some more to understand how to tackle it. Had I given in to distractions, it would have taken more time, and the solution would have been inferior. Not to count the frustration of reloading information in my mental RAM at each context switch.
