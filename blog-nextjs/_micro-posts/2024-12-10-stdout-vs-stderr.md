---
title: Stdout vs stderr
description:
author: Riccardo
tags:
  - Software
---

Use `stdout` for the output of your program: the log in `git log`, the matching files with `find`, or the list from `ls`.

Use `stderr` for errors AND ANYTHING ELSE: the progress messages, the warns/info, and the jokes (if you really need to).

For example, given the following output (on `stdout`):

```
I, [2024-12-10T18:59:45.134882 #7388]  INFO -- : Crystalball starts to glow...
W, [2024-12-10T18:59:45.138463 #7388]  WARN -- : Maps are outdated!
I, [2024-12-10T18:59:45.311953 #7388]  INFO -- : Starting RSpec.
{"version":"3.13.2","messages":["No examples found."],"seed":41594,"examples":[],"summary":{"duration":6e-05,"example_count":0,"failure_count":0,"pending_count":0,"errors_outside_of_examples_count":0},"summary_line":"0 examples, 0 failures"}
```

I had to remove manually the first lines to parse the JSON:

```bash
command | tail -n1 | jq
```
