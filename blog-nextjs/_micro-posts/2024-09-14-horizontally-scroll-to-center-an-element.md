---
title: Horizontally scroll to center an element
author: Riccardo
tags:
  - JavaScript
---

```html
<div class="width-full overflow-x-auto">
  <div>...</div>
  <div>element</div>
  <div>...</div>
</div>
```

```js
const viewportWidth =
  Math.max(document.documentElement.clientWidth, window.innerWidth || 0)

container.scrollLeft =
  element.offsetLeft // scroll to the left edge of element
  + element.offsetWidth / 2 // scroll to center of element
  - viewportWidth / 2 // scroll back half the viewport
```
