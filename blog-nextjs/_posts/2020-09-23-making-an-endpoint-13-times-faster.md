---
title: Making an Endpoint 13 Times Faster
description: Down to 900 milliseconds. In other words, more than 13 times faster in the worst case (before it took 12 seconds) and infinitely faster in the best case (no more crashes).
published: true
author: Riccardo
cover_image: https://odone.io/images/running.jpg
tags:
  - Essential Skills
---

Last week I prepared a [script to reproduce performance issues](https://odone.io/posts/2020-09-16-how-to-investigate-performance-issues-in-a-web-app-with-a-simple-script/). The goal was to populate the database with realistic data to test speed and UX on development and staging. As a result, we realized **some screens in the application not only are slow, but they also break when displaying a big dataset**.

We started working on the page that lists all the orders submitted by customers. The script inserts in the database an incremental number of them. Since in the screen it's possible to filter by date, this setup allowed us to **check how fast the application loads compared to the number of items**:

- 25 orders took about 8 seconds;
- 50 orders took about 12 seconds;
- 100 orders took about 18 seconds;
- around 1000 orders, the application crashed with "Memory quota vastly exceeded".

As a developer, my first instinct was to jump in and make it fast. But there were two issues with that approach: identifying **what problem to solve**, and **defining what *fast* means**.

Even if it were technically feasible **loading thousands of items fast enough, would it even make sense?** In our case, we don't think so. Given our understanding of the users, we believe they would interact with the list by using the search field to drill down to one specific order. In other words, they would rarely scroll the entire list, especially when hundreds of items were displayed.

By taking a look at analytics, we identified **where the application spends time** rendering the list:

- about 85% on the backend (more than 90% time is spent in the JSON serializer);
- about 15% on the frontend.

Given this is a [rescue project](https://odone.io/posts/2020-07-10-grateful-for-the-opportunity-of-working-on-legacy-code/), we are aiming for good enough, not perfect. But **what does fast enough mean in our context?**

We went back to staging and loaded different amounts of orders on the screen to get a feel for it. There are more scientific ways of defining a performance budget, but we didn't want to overengineer.

With the user's need in mind, understanding the performance issues, and a definition of fast, I decided to explore solutions by **spiking code**. I had assumptions on what and how to optimize, but I wanted to validate the solution before going all-in.

I coded the JSON serializer from scratch, removed the unneeded data sent to the browser, paginated the endpoint, and removed the n+1 queries. This got us to 900 milliseconds, regardless of the search criteria. Since each page contains 50 items, we are **more than 13 times faster** in the worst case (i.e., 12000/900) and **infinitely faster** in the best case (i.e., the application does not crash).

The spike was successful. Now it's time to save the git diff, reset hard, and rewrite from scratch with the proper discipline.
