---
title: The love-hate relationship between SELECT and GROUP BY in SQL
description: A clear explanation of the relationship between SELECT and GROUP BY in SQL guided by examples.
published: true
author: Riccardo
cover_image: https://cdn-images-1.medium.com/max/1600/1*C9Ly9V1lpo-yW9eBawUQEw.jpeg
canonical_url: https://medium.com/@riccardoodone/the-love-hate-relationship-between-select-and-group-by-in-sql-4957b2a70229
tags:
  - Essential Skills
---

This post from 2017 keeps getting love on Medium, so I decided to crosspost it here.

## The problem

I was recently working on some database queries (in PostgreSQL) and encountered the following error:

```
ERROR: column "MY_TABLE.MY_COLUMN" must appear in the GROUP BY clause or be used in an aggregate function
```

I felt ashamed not being able to explain in a clear manner why that happened. Especially, when a colleague reminded me that in MySQL that works (scroll at the bottom to read more about it).

## GROUP BY in SQL

They say the best way to understand anything is teaching it, so here I am. But first let's backtrack a bit.

Let's say we have two tables: `films` and `reviews` with the following schemas:

- `films`: `id` (int), `title` (string), `genre` (string);
- `reviews`: `id` (int), `film_in` (int), `content` (string).

Also, `films` contains the following records:

```sql
=> SELECT id, genre, title from films;

 id |  genre   |       title
----+----------+-------------------
  1 | thriller | Blade Runner 2049
  2 | thriller | Inception
  3 | scifi    | Arrival
```

And `reviews`:

```sql
=> SELECT id, film_id, content FROM reviews;

 id | film_id |                          content
----+---------+-----------------------------------------------------------
  1 |       1 | Visually stunning and thought provoking, but not flawless
  2 |       1 | One of the best sequels of all time
  3 |       1 | Strangely boring, lacking tension and intelligence
  4 |       2 | Amazing Directing, Captivating Plot, Overall Great
```

Let's say we want to count how many movies of one specific genre are. This is where `GROUP BY` comes handy. In fact, it allows grouping the result-set by one or more columns and calculate aggregate functions on the grouped records:

```sql
=> SELECT genre, count(*) FROM films GROUP BY genre;

  genre   | count
----------+-------
 thriller |     2
 scifi    |     1
```

In the previous query, `count` indicates how many records with the same value in the column `genre` are in `films`: two thrillers (i.e. Blade Runner 2049 and Inception) and one scifi (i.e. Arrival). In other words, `count` represents how many records were grouped together by genre.

Let's say we wanted to know which titles belong to each genre group. We could try with:

```sql
=> SELECT title, genre, count(*) FROM films GROUP BY genre;

ERROR:  column "films.title" must appear in the GROUP BY clause or be used in an aggregate function
LINE 1: SELECT title, genre, count(*) FROM films GROUP BY genre;
```

Except we cannot. In fact, there's no way for the database engine to select a `title` since there could be more than one record with the same `genre`. Problem that is actually present in our example: there are two thrillers.

Well, we could blindly add the `title` column to the `GROUP BY` as the first part of the error suggest, correct?

```sql
=> SELECT title, genre, count(*) FROM films GROUP BY title, genre;

       title       |  genre   | count
-------------------+----------+-------
 Blade Runner 2049 | thriller |     1
 Inception         | thriller |     1
 Arrival           | scifi    |     1
```

Wrong, this is not the query we want to execute.

It's the second part of the error what we are looking for (i.e., `or be used in an aggregate function`). In fact, there's a set of aggregate functions that databases offer us. In PostgreSQL `count()` is one example we've already seen, but there's a lot more.

In our case, `string_agg()` is a good choice:

```sql
=> SELECT string_agg(title, ', ') as titles, genre, count(*) FROM films GROUP BY genre;

            titles            |  genre   | count
------------------------------+----------+-------
 Blade Runner 2049, Inception | thriller |     2
 Arrival                      | scifi    |     1
```

## A formal definition of GROUP BY

Up until now we have seen that in the `SELECT` statement we can have only columns that either appear in the `GROUP BY` or are arguments of an aggregate function.

Well, this is not the whole truth. In [Roland Bouman's words](http://rpbouman.blogspot.de/2007/05/debunking-group-by-myths.html) "the 1999 and 2003 versions of the SQL standard require that the columns appearing in the `SELECT` list are functionally dependent upon the groups defined by the `GROUP BY` clause. In other words, if we know that a column contains only one value for any given combination of values in the columns appearing in the `GROUP BY` clause, we may reference the column in the `SELECT` list even if it does not appear in an aggregate expression".

For example, a primary or unique key satisfies that rule. In fact, by selecting one value of a unique key, the value of the other columns have only one combination of values. In other words, there's only one record with that specific value in the unique key. Or better yet, the values in the other columns are functionally dependent upon the unique key.

Therefore, since `id` is a primary key (i.e. unique by definition) all other columns in the same table are functionally dependent upon it. That's why the following query does not raise any errors:

```sql
=> SELECT id, title, genre FROM films GROUP BY id;

 id |       title       |  genre
----+-------------------+----------
  3 | Arrival           | scifi
  1 | Blade Runner 2049 | thriller
  2 | Inception         | thriller
```

Of course, grouping by a unique column when querying one table does not make any sense. But it comes useful when other tables are involved:

```sql
=> SELECT films.id, films.title, films.genre, count(*) as number_of_reviews FROM films LEFT JOIN reviews ON films.id = reviews.film_id GROUP BY films.id;

 id |       title       |  genre   | number_of_reviews
----+-------------------+----------+-------------------
  3 | Arrival           | scifi    |                 1
  1 | Blade Runner 2049 | thriller |                 3
  2 | Inception         | thriller |                 1
```

## GROUP BY in MySQL

It's interesting to notice that MySQL didn't historically conform to SQL standards. As a matter of fact, the engine let you `SELECT` anything in a query with a `GROUP BY`. Including non-aggregated columns that do not appear in the `GROUP BY` and that are not functionally dependent upon the `GROUP BY` clause:

```sql
mysql> SELECT title, genre, count(*) FROM films GROUP BY genre;

+-------------------+----------+----------+
| title             | genre    | count(*) |
+-------------------+----------+----------+
| Arrival           | scifi    |        1 |
| Blade Runner 2049 | thriller |        2 |
+-------------------+----------+----------+
```

As you can see, the engine picked a random record out of the thriller group. This is concerning and I believe databases should at least warn users in that case.

Luckily, the correct behaviour can be enabled by adding a flag (i.e., `ONLY_FULL_GROUP_BY`) or by using MySQL v5.7.5 or higher. In that case, the query would raise the following error:

```sql
mysql> SELECT title, genre, count(*) FROM films GROUP BY genre;

ERROR 1055 (42000): Expression #1 of SELECT list is not in GROUP BY clause and contains nonaggregated column 'some_database.films.title' which is not functionally dependent on columns in GROUP BY clause; this is incompatible with sql_mode=only_full_group_by
```

## Pointers

All you need to know can be found in this in-depth post by Roland Bouman: [Debunking GROUP BY myths](http://rpbouman.blogspot.de/2007/05/debunking-group-by-myths.html).
