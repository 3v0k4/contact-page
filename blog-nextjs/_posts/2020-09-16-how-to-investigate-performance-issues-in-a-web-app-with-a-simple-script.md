---
title: How to Investigate Performance Issues in a Web App with a Simple Script
description: If only I could simulate a user but faster. Wait a second! A person clicking things in a browser just sends HTTP requests to the server.
published: true
author: Riccardo
cover_image: https://odone.io/images/gym.jpg
tags:
  - Essential Skills
---

> What happens when you build for several months without testing with real users and real data? An application that doesn't work with real users and real data happens.

This is what I wrote in my [rant about 99 Percent Done](https://odone.io/posts/2020-09-10-99-percent-done.html). But I'm tasked with being [part of the solution not part of the problem](https://odone.io/posts/2020-07-10-grateful-for-the-opportunity-of-working-on-legacy-code.html). So let's make performance right.

The first step to solve any coding issue is to be able to reproduce at will. In this case, the endpoints are slow, and I'm ready to bet all-in that it's due to database calls and JSON rendering. Not only that, the application monitoring service we employ draws the same conclusions. Thus, I could jump right in, gold master test, and rewrite queries and serializers. However, there are a couple of problems with this approach.

First of all, I would need to populate the database with enough data to perform a meaningful performance (and UX) test. It's not feasible to fill thousands of forms by hand to reproduce every time. A small, unrealistic dataset would not work either for obvious reasons.

Secondly, I don't want to optimize the entire application. It would take forever, and, at the moment, we are validating the product, not perfecting it. Said differently, I want to fix performance problems in places where users will spend most of their time, not optimize a screen they will rarely use.

Lastly and most importantly, I need to share performance issues with the rest of the team. We have to experience features to decide when they are both fast enough and usable. Technically the application could render thousands of things on the screen, but the user experience would likely suffer.

Given the [shake list](https://odone.io/posts/2020-08-28-how-to-tame-complexity-into-simplicity-with-a-shake-list.html) mentioned above, I got a bunch of options:

- Tweak performance without any feedback. Nope.

- Manually fill thousands of forms. Hell, no!

- Some sort of database seeds on development and staging. This would yield the most fine-grained control. In fact, too much. I would have to understand the complex network of records each form submission generates and recreate it manually. Too error-prone and time-consuming.

- Call controllers behind the endpoints directly. This seems sexy because I wouldn't need to get acquainted with implementation details, but mingling with the web framework's internals feels hacky.

If only I could simulate a user but faster. Wait a second! A person clicking things in a browser just sends HTTP requests to the server.

I can copy the requests from the network tab in the dev tools, translate them into code, and wrap each request in loops and conditionals. Moreover, there are services to translate cURL commands into another language (search "curl to MY\_LANGUAGE"), and copying cURL requests is one right-click away:

![Screenshot of the menu that appears when right-clicking a request in the dev tool's network tab](https://odone.io/images/network.png)

Too good to be true, let's analyze the cons:

- Coarser-grained control. I can either call an endpoint or not; there's no middle ground. This is a plus because it's what actual users do. Also, if I need to interact with the database, I could connect to it without a problem.

- Automated emails. Luckily the framework allows disabling all deliveries with a boolean flag.

- Third-party API calls (e.g., payments and tax calculations). I'm in luck again because orders can be marked as tax-free and paid in cash to avoid third-party service requests. This makes things a bit less realistic, but we could manually test those specific cases or develop more sophistication.

- Authentication. The application uses tokens, so with a couple of additional requests, I can authenticate the script.

Great, there are no blockers. On the plus side, there are a ton of pros:

- Easy to add new requests. Open the browser, simulate the user, copy the cURL, paste them in the script. We could even ask users to send that along as a way to reproduce things.

- Test memory leaks. Among other issues, this application leaks memory, and the script can help investigate that problem.

- Test concurrency. Requests could be performed in parallel to load test the application as described in [Facing Concurrency in Web Apps Guided by Tests](https://medium.com/@riccardoodone/facing-concurrency-in-web-apps-guided-by-tests-3f5488f62607).

- Measure. I could time each request to verify the performance tweaks do work.

- Feedback. Anyone on the team can navigate to a screen and feel for its speed and usability.

- We are the users. Performance can be measured into a single number, but UX is interaction, not math, at the end of the day. By putting ourselves in the shoes of the customers, we can make better decisions.

This looks promising. I'll let you know in the [newsletter](https://odone.io/#newsletter) whether I crashed and burned or if I can move to rant about the next legacy issue in this codebase.
