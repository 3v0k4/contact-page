---
title: Find unused indexes in Postgres
author: Riccardo
description: How to use the `pg_stat_user_indexes` table
tags:
  - Postgres
---

`SELECT * FROM pg_stat_user_indexes WHERE relname = 'TABLE_NAME'`:
- `idx_scan`: how many times the index was used
- `idx_tup_read`: how many index entries were returned using the index
- `idx_tup_fetch`: how many rows were returned using the index

You want:
- `idx_scan` to move up (ie, the index is used)
- `idx_tup_read`/`idx_tup_fetch` to move up but not too much (ie, the index is highly selective)

Source: [sgerogia.github.io/Postgres-Index-And-Queries/](https://sgerogia.github.io/Postgres-Index-And-Queries/)
