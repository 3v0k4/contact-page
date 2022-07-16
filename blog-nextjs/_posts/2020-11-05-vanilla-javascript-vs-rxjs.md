---
title: Vanilla JavaScript vs. RxJs
description: 'Tackling problems from different angles: imperative against declarative, or "push" vs. "pull."'
published: true
author: Riccardo
cover_image: https://odone.io/images/hacker.jpg
tags:
  - Essential Skills
---

This post compares vanilla JavaScript with RxJs. My intent is not to demonstrate whether one or the other is the best approach. As always, it depends.

But I want to highlight the importance of **tackling a problem from different angles**. In this case, it's imperative against declarative, or "push" vs. "pull."

Also, different mental models provide insights that can be exploited in the solution, regardless of the paradigm chosen. In this article, the imperative approach helps exploring the problem, the declarative one distills the solution: both have their merits.

## It's Monday Morning

While you wait for the browser to load the to-dos, you wonder about the feature you will be working on today.

Maybe you will work in Elm-land where run-time exceptions never show up, or you will be modeling new domains in Haskell where impossible states don't compile.

Nope, it's JavaScript. You need to add an input field to enable users to fetch data.

*Damn*.

You believe in small steps and short feedback loops, so this is your first move:

```html
<input type="text" id="query" />
```

```js
const callback = value => console.log(value)

const queryElement = document.getElementById("query")
queryElement.addEventListener('input', event => callback(event.target.value))
```

A glance at the browser confirms that typing in the input field logs the value in the console. Great!

Time to fetch:

```diff
-const callback = value => console.log(value)
+const callback = query =>
+  fetch(`https://httpbin.org/get?query=${encodeURIComponent(query)}`)
+    .then(response => response.json())
+    .then(response => console.log(response))
```

Another quick manual test confirms that the requests work.

You spend the rest of the day making things pretty and replacing the `console.log()` with the appropriate function to fill the DOM. Then, you move the ticket to done full of pride.

*That was slick!*

Unfortunately, the next day you get an email from the devops team with the following subject: **URGENT!1!**. After your deploy, servers started receiving a ton of requests.

You open the application and type "holy moly!" in the text field. Your heart skips a bit when you notice it generated 10 network requests:

- "h"
- "ho"
- "hol"
- "holy"
- "holy "
- "holy m"
- ...

*Holy moly! indeed, I forgot to debounce!*

```diff
+const DEBOUNCE_MILLISECONDS = 300
+let scheduled
+
 const callback = query =>
   fetch(`https://httpbin.org/get?query=${encodeURIComponent(query)}`)
     .then(response => response.json())
     .then(response => console.log(response))

+const debounce = fnc => arg => {
+  clearTimeout(scheduled)
+  scheduled = setTimeout(() => fnc(arg), DEBOUNCE_MILLISECONDS)
+}
+
+const debouncedCallback = debounce(callback)
+
 const queryElement = document.getElementById("query")
-queryElement.addEventListener('input', event => callback(event.target.value))
+queryElement.addEventListener('input', event => debouncedCallback(event.target.value))
```

To make sure not to piss the ops team again, you get deeper into manual testing. The debouncing works, but there is something strange: sometimes, the application displays data for an old query.

*Aha, the responses are coming out of order.*

To make it more visible you introduce a random delay in the `fetch`:

```diff
+const throttledFetch = (url, options) => {
+  return new Promise((res, rej) => {
+    const throttleBy = Math.random() * 10000
+    console.log(`throttledBy ${throttleBy} milliseconds`)
+    fetch(url)
+      .then(x => setTimeout(() => res(x), throttleBy))
+      .catch(x => setTimeout(() => rej(x), throttleBy))
+  })
+}
+
 const callback = query =>
-  fetch(`https://httpbin.org/get?query=${encodeURIComponent(query)}`)
+  throttledFetch(`https://httpbin.org/get?query=${encodeURIComponent(query)}`)
     .then(response => response.json())
     .then(response => console.log(response))
```

Luckily, you can abort the previous `fetch` before executing the next one:

```diff
+let controller = new AbortController()

 const throttledFetch = (url, options) => {
   return new Promise((res, rej) => {
     const throttleBy = Math.random() * 10000
     console.log(`throttleBy ${throttleBy} milliseconds`)
-    fetch(url)
+    controller.abort()
+    controller = new AbortController()
+    fetch(url, { signal: controller.signal })
```

It's almost the end of the day, and you are staring at this code:

```js
const DEBOUNCE_MILLISECONDS = 300
let scheduled
let controller = new AbortController()

const throttledFetch = (url, options) => {
  return new Promise((res, rej) => {
    const throttleBy = Math.random() * 10000
    console.log(`throttleBy ${throttleBy} milliseconds`)
    controller.abort()
    controller = new AbortController()
    fetch(url, { signal: controller.signal })
      .then(x => setTimeout(() => res(x), throttleBy))
      .catch(x => setTimeout(() => rej(x), throttleBy))
  })
}

const callback = query =>
  throttledFetch(`https://httpbin.org/get?query=${encodeURIComponent(query)}`)
    .then(response => response.json())
    .then(response => console.log(response))
    .catch(error => console.log(error))

const debounce = fnc => arg => {
  clearTimeout(scheduled)
  scheduled = setTimeout(() => fnc(arg), DEBOUNCE_MILLISECONDS)
}

const debouncedCallback = debounce(callback)

const queryElement = document.getElementById("query")
queryElement.addEventListener("input", event => debouncedCallback(event.target.value))
```

The throttling code needs to be removed. Still, the software crafter inside your head is in pain. You shouldn't have to tell JavaScript what to do line by line.

Instead of "pushing" information around, you want to "pull" and react to it. It should be as declarative as a spreadsheet.

It's too late to conjure that thought, your fingers are already typing `yarn add rxjs`:

```js
const queryElement = document.getElementById("query")

fromEvent(queryElement, 'input').pipe(
  debounceTime(300),
  map(event => event.target.value),
  switchMap(query => fromFetch(`https://httpbin.org/get?query=${encodeURIComponent(query)}`)),
  flatMap(response => response.json()),
  catchError(error => console.log(error))
)
.subscribe(response => console.log(response))
```

Not only this achieves the same result, but also it's shorter and declarative. Not to count the additional insight you notice from this new angle:

```diff
const queryElement = document.getElementById("query")

fromEvent(queryElement, 'input').pipe(
  debounceTime(300),
  map(event => event.target.value),
+ distinctUntilChanged(),
  switchMap(query => fromFetch(`https://httpbin.org/get?query=${encodeURIComponent(query)}`)),
  flatMap(response => response.json()),
  catchError(error => console.log(error))
)
.subscribe(response => console.log(response))
```

You make sure nobody else is looking, you sneak in the additional dependency, and you deploy.

Now, it's the end of the day!
