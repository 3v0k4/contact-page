---
title: HTTP cache cookbook
description:
author: Riccardo
tags:
  - Http
---

![Flow chart: Reusable response? No: `no-store`. Yes: Revalidate each time? Yes: `no-cache` and continue to No: Cacheable by intermediate caches? No: `private` and continue. Yes: `public` and continue. Maximum cache lifetime? `max-age=x` & use `ETag`.](https://github.com/user-attachments/assets/ea2a9b70-2b4e-443e-b6e1-808ee3dd3a83)

Examples:
- Fingerprinted URLs
  - Want: allow intermediate caches (not only browser) to cache for a year and after that revalidate with ETag
  - Use: `Cache-Control: public, max-age=31536000` and configure `ETag` header in server's responses
- Mutable content
  - Want: Always revalidate with ETag
  - Use: `Cache-Control: `no-cache` AND configure `ETag`

**What's the difference between `no-cache` vs `must-revalidate` + `max-age`?**

- `no-cache` requires client to ALWAYS check if content is fresh (e.g., with `ETag` or `Last-Modified`)
- `must-revalidate` + `max-age` allows the cache to serve cached content until `max-age` is reached

**Why using `Cache-Control: must-revalidate, max-age=600` is a bad idea for mutable content?**

If that Url serves an Html that fetches additional assets (e.g., Js, Css) with the same cache strategy, the cache may load Html and Css and the server may load Js which refers to the newer (not cached) Html and Css.

Source: [Prevent unnecessary network requests with the HTTP Cache](https://web.dev/articles/http-cache)
