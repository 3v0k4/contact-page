---
title: Install an old version of a Brew formula
description:
author: Riccardo
tags:
  - macOS
---

```bash
HOMEBREW_NO_INSTALL_FROM_API=1 brew tap --force homebrew/core
cd $(brew --repository homebrew/core)
git log --oneline --follow Formula/g/gdb.rb
git checkout <sha>
brew install ./Formula/g/gdb.rb
```

Source: https://gist.github.com/mike-myers-tob/9a6013124bad7ff074d3297db2c98247?permalink_comment_id=5407666#gistcomment-5407666
