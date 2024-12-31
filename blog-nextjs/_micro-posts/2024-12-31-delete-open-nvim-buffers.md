---
title: Delete open Nvim buffers
description:
author: Riccardo
tags:
  - Nvim
---

List all open buffers:

```
:ls
```

Close all open buffers:

```
%bd!|e#|bd#
```

- `%bd!` foreach buffer delete
- `e#` open the last buffer for editing
- `bd#` delete the \[No Name\] buffer

Source: https://stackoverflow.com/questions/4545275/vim-close-all-buffers-but-this-one
