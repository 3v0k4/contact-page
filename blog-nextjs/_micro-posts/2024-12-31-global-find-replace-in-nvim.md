---
title: Global find/replace in Nvim
description:
author: Riccardo
tags:
  - Nvim
---

Fill the quickfix list with something like [vim-grepper](https://github.com/mhinz/vim-grepper) and:

```
cdo %s/BEFORE/AFTER/gc
```

`cdo` executes what comes after for each entry in the quickfix list (or use `cfdo` to run once per file).
