---
title: Elm Tricks from Production–Adding Event Listeners to DOM Nodes that do not yet Exist
description: How to use `setTimeout` and the DOM mutation observer API to attach callbacks to DOM nodes that do not yet exist.
published: true
author: Riccardo
cover_image: https://odone.io/images/elm.jpg
canonical_url: https://blog.lunarlogic.io/2019/elm-tricks-from-production-adding-event-listeners/
series: Elm Tricks from Production
tags:
  - Functional Programming
  - Elm
---

There are some situations where it is required to attach a callback to a DOM node which does not yet exist. There are multiple solutions to this problem. In this post we will talk about the two we employed in [AirCasting](http://aircasting.org/).

If the DOM node is supposed to be rendered soon after the web page is loaded, it is possible to “loop” until the element appears. For example, AirCasting allows users to modify the to colour code thresholds of measurements shown on the map by interacting with the slider at the bottom of the page (aka Heatmap):

![Screenshot of AirCasting with the heatmap indicated](/images/aircasting_heatmap_elm.png)

Notice that the slider has multiple handles, to make this happen we use the [“noUiSlider”](https://refreshless.com/nouislider/) library. The section of the page where the slider should be mounted is rendered by Elm. This is a problem because the JavaScript code that initializes Elm is the same that bootstraps the Heatmap slider. We solved that problem by [using a `setTimeout`](https://github.com/HabitatMap/AirCasting/blob/320401a6fc83c57cd4436153e5744d6655a1e450/app/javascript/packs/elm.js#L144) to wait for the slider container to appear in the DOM:

```js
const setupHeatMap = () => {
  const node = document.getElementById("heatmap");
  if (!node) {
    setTimeout(setupHeatMap, 100);
  } else {
    // setup heatmap
  }
}
```
In some cases the DOM node we want to append a callback to appears as a result of a user interaction. In this instance, it doesn’t make sense to use `setTimeout`. In fact, there’s a chance that the interaction would happen much later or never. Not to mention the fact that the DOM element could appear and disappear multiple times.

Luckily, we can use the [“Mutation Observer” API](https://developer.mozilla.org/en-US/docs/Web/API/MutationObserver):

> The MutationObserver interface provides the ability to watch for changes being made to the DOM tree.

In [AirCasting](http://aircasting.org/) we use the MutationObserver to enable users to scroll the sessions list horizontally when scrolling vertically with a mouse wheel.

![Screenshot of AirCasting with the sessions list indicated](/images/aircasting_sessions_list_elm.png)

We could not use the `setTimeout` strategy because the sessions list disappears whenever a session is selected. Also, if a user opened the application using a link to a selected session, the `setTimeout` would keep looping until the session was deselected.

For the reasons mentioned above, we decided to use the DOM Mutation Observer API via a [wrapper](https://gist.github.com/pablen/c07afa6a69291d771699b0e8c91fe547) written by [pablen](https://gist.github.com/pablen). The gist provides the code and an example of how to use it.

In our case, we want to [setup the scroll behaviour](https://github.com/HabitatMap/AirCasting/blob/320401a6fc83c57cd4436153e5744d6655a1e450/app/javascript/packs/elm.js#L262) as soon as the sessions list appears on the screen:

```js
const setupHorizontalWheelScroll = node => {
  // …
};

createObserver({
  selector: ".session-cards-container",
  onMount: setupHorizontalWheelScroll
});
```

Notice that we keep adding the callback whenever `.session-cards-container` appears. We don’t need to unregister the callback since the DOM node is discarded whenever a session is selected and the container disappears.
