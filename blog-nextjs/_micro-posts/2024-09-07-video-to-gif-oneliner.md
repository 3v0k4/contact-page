---
title: Video to Gif oneliner
author: Riccardo
description: Using `ffmpeg` to transform an .mp4 into a .gif
tags:
  - Gif
---

This is how I created the demos for [3v0k4/exit.nvim](https://github.com/3v0k4/exit.nvim):
- Recorded the screen with QuickTime into an .mp4
- Transformed into a .gif with `ffmpeg`

```bash
ffmpeg \
  -i input.mp4 \
  -vf "fps=10,scale=640:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" \
  -loop 0 \
  output.gif
```

Source: [SuperUser](https://superuser.com/a/556031)
