---
title: Size of a column in Postgres
description:
author: Riccardo
tags:
  - Postgres
---

[`pg_column_size ( "any" ) → integer`](https://www.postgresql.org/docs/current/functions-admin.html#FUNCTIONS-ADMIN-DBOBJECT) "Shows the number of bytes used to store any individual data value. If applied directly to a table column value, this reflects any compression that was done."

Example:

```sql
SELECT name, pg_column_size(name) FROM table
```
