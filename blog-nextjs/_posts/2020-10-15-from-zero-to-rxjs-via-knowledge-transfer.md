---
title: From Zero to RxJs via Knowledge Transfer
description: When I'm exposed to a new concept close to a dense area of my knowledge graph, I can approach it from multiple angles using different mental models.
published: true
author: Riccardo
cover_image: https://odone.io/images/shuttle.jpg
tags:
  - Essential Skills
series: Permanent Knowledge
---

A couple of weeks ago, I talked about going [From Temporary Knowledge to Permanent Knowledge](https://odone.io/posts/2020-10-01-from-temporary-knowledge-to-permanent-knowledge/). Last week, I shared [10 Knowledge Transfers that Make me Thrive with Legacy Code](https://odone.io/posts/2020-10-09-10-knowledge-transfers-that-make-me-thrive-with-legacy-code/). Today, I want to ride the momentum and focus on how permanent knowledge got me up to speed with RxJs.

RxJs is a complex library. It's confusing because it requires solving problems differently. In particular, it makes heavy use of four concepts that are not straightforward by themselves: async programming, streams, event-driven modeling, and functional programming.

RxJs is incredibly powerful and declarative, though. Let me shamefully steal some code from [rxjs.dev](https://rxjs.dev/guide/overview) to show why:

```js
// Vanilla JavaScript

let count = 0;
const rate = 1000;
let lastClick = Date.now() - rate;
document.addEventListener('click', event => {
  if (Date.now() - lastClick >= rate) {
    count += event.clientX;
    console.log(count);
    lastClick = Date.now();
  }
});

// RxJs

fromEvent(document, 'click')
  .pipe(
    throttleTime(1000),
    map(event => event.clientX),
    scan((count, clientX) => count + clientX, 0)
  )
  .subscribe(count => console.log(count));
```

At the cost of increased complexity, RxJs subscribes to a stream of click events, employing local state, and composing declarative code with functional programming trickery. Said differently, the vanilla JavaScript code models the evolution of a value over time by mutating (global) state.

I would posit, complexity is there regardless; in one case, it's in using RxJs; in the other, it's embedded in the vanilla JavaScript code. But I digress.

Hopefully, the example justifies the need for a different approach when writing RxJs code. However, I quickly managed to get up to speed by transferring knowledge from the past and filling in the blanks.

It's similar to those RPG games where the map is black until the area is explored. I had already visited most of the places surrounding the principles RxJs builds upon. These mental models allowed me to approach the library with ease. Let's see how.

## Streams

In RxJs, everything is a value that evolves over time. It's like an array of elements that are appended eventually. That is why, in the example above, it's possible to `map` to transform each value, including the future ones!

In other words, arrays support a pull use-case while streams a push use-case. If we needed to see the room temperature in real-time, the former would require polling and state mutations; the latter would just work.

Great, modulo some quirks streams can be treated like arrays.

## Async Programming

I still remember the confusion when I started working with promises (or futures). I could not reconcile sync and async programming:

```js
var result = fetch(...)
console.log(result) // WRONG: it logs the promise
```

Then, I realized when you go async, you cannot go back:

```js
fetch(...).then(result => console.log(result))
```

If a promise produces a value eventually, streams produce multiple values eventually. But everything is async, so the former requires `then`, the latter provides several operators (see the initial example).

Awesome, streams are like promises.

## Event-Driven Modelling

Visualizing a problem in terms of events is strange. Luckily, I was exposed to DDD and Event Storming early in my career. However, we all know a perfect example of event-driven programming: spreadsheets.

In particular, to calculate in C1 the addition of A1 and A2, it's enough to use `=A1+A2`. That is, whenever A1 or A2 changes, update the cell to their sum. Notice that nobody "pushes" values to C1; it's C1 that "pulls" from A1 and A2. I find it elegant and clean.

Nice, event-driven programming is like using spreadsheets.

## Functional Programming

RxJs employs functional programming for flow control, composition, and decomposition. On top of that, it adheres to many interfaces (typeclasses) that make me feel at home.

By looking at the [PureScript bindings for RxJs](https://pursuit.purescript.org/packages/purescript-rx/2.0.0/docs/Rx.Observable), it's clear that an Observable is a Functor, a Monad, a Semigroup, and many others. That is enough to know I can use `map`, `flatMap`, `concat`, and more.

Hell yeah, learning monads was not time wasted!

## Outro

Hopefully, this explains how knowledge transfer allowed me to rock and roll with RxJs.

Notice that permanent knowledge compounds. When I'm exposed to a new concept close to a dense area of my knowledge graph, I can approach it from multiple angles making sense of it with different mental models.
