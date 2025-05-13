---
title: Only run added/modified tests
description:
author: Riccardo
tags:
  - Git
---

```bash
git diff --name-only --diff-filter=AM -- 'spec/*_spec.rb' | xargs bin/spring rspec
```
