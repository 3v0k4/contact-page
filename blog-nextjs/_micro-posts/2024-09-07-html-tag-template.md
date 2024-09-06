---
title: Html `<template>`
author: Riccardo
description: Hidden and re-usable pieces of Html
tags:
  - Html
---

In some situations, you may want to render some hidden Html that can be later accessed by JavaScript to use as a template.

For example, a form to create a survey with a title and multiple questions:

```
<form>
  <input type="text" name="title" placeholder="Enter title" />
  <button type="button" onclick="addQuestion()">Add question</button>
  <p>Question</p>
  <input type="text" name="question" placeholder="Enter question" />
</form>

<template>
  <p>Question</p>
  <input type="text" name="question" placeholder="Enter question" />
</template>

<script>
  function addQuestion() {
    const template = document.querySelectorAll("template")[0].content;
    const clone = template.cloneNode(true);
    document.querySelectorAll("form")[0].appendChild(clone);
  }
</script>
```

What I used in the past is less elegant:

```
<form>
  <input type="text" name="title" placeholder="Enter title" />
  <button type="button" onclick="addQuestion()">Add question</button>
  <p>Question</p>
  <input type="text" name="question" placeholder="Enter question" />
</form>

<div class="template" style="display: none;">
  <p>Question</p>
  <input type="text" name="question" placeholder="Enter question" />
</div>

<script>
  function addQuestion() {
    const template = document.querySelectorAll(".template")[0];
    const clone = template.cloneNode(true);
    clone.style.display = "block";
    document.querySelectorAll("form")[0].appendChild(clone);
  }
</script>
```
