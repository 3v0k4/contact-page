---
title: Render Html, Css, and Js from a GitHub gist
description:
author: Riccardo
tags:
  - GitHub
---

You can use `https://gistpreview.github.io/?GIST_ID` to render the `index.html` from a gist:

- [https://gist.github.com/3v0k4/47fcdc2dfc1661cfb240748fc0d97f1b](https://gist.github.com/3v0k4/47fcdc2dfc1661cfb240748fc0d97f1b)
- [https://gistpreview.github.io/?47fcdc2dfc1661cfb240748fc0d97f1b](https://gistpreview.github.io/?47fcdc2dfc1661cfb240748fc0d97f1b)

Use `https://gistpreview.github.io/?GIST_ID/FILE_NAME` for any file:
- [https://gistpreview.github.io/?47fcdc2dfc1661cfb240748fc0d97f1b/index.html](https://gistpreview.github.io/?47fcdc2dfc1661cfb240748fc0d97f1b/index.html)

The raw content of a file (e.g., PNG) can be accessed with `https://gist.github.com/3v0k4/GIST_ID/raw/BLOB_ID/FILE_NAME`. To get the `BLOB_ID` use `git ls-tree -r head` in the [cloned gist](https://odone.me/micro-posts/2025-08-22-clone-and-push-to-a-github-gist/).
