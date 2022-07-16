---
title: The Simplest Most Powerful Trick–Verification Steps
description: Verification steps don't come with a 100% save-ass guarantee, but the list of benefits is long, and it's not limited to finding bugs.
published: true
author: Riccardo
cover_image: https://odone.io/images/steps.jpg
tags:
  - Essential Skills
  - Team Work
---

Manual testing is an essential piece in the software development puzzle. Still, it's often not given the respect it deserves. Sometimes, teams work without any quality assurance and don't test at all. Sometimes, it's the author of the feature who half-asses manual testing. Sometimes, it's the client that clicks around the interface without any guidance.

Sure, it would be best to have an expert tester on the team. Still, not having one should not mean disregarding manual testing. If you are reading this article, I bet you are not a chef, but you manage to eat every day. Instead, we should enable everybody to perform manual testing. Even if the team can count on a dedicated tester.

Some years ago, I was exposed to an equally simple and powerful idea: **verification steps**.

![Screenshot of verification steps out of a Trello ticket. Verification steps: 1. login on staging with riccardo.odone 2. open reporting 3. go to production checklist 4. notice it's fast enough 5. select dates to cover September (14th has 25 orders, 15th has 50 orders, etc.) 6. notice the pagination (50 per page) 7. wait a few seconds 8. notice the print button becomes active 9. notice that printing would print all pages not only the current one 10. notice, both in the page and in printing, items are ordered by fulfillment date first, order number second (Please check with other accounts too, Please do a light testing of manage orders and tax summary–just visualizing them not generating)](https://odone.io/images/verification-steps.png)

Verification steps provide guidance on **how to verify a feature implementation**. They look similar to a description of how to reproduce an issue in a bug report. They don't come with a 100% save-ass guarantee, but the list of benefits is long, and it's not limited to finding bugs:

- **Summary**. Ticket descriptions tend to be in the form of a conversation, not to count all the back-and-forth in the comments. Verification steps work as a TL;DR.

- **BDD**. As a developer, I love diving into code right away. Sometimes it means I get lost in the details and lose sight of the big picture. Having a clear step-by-step breakdown in the ticket helps.

- **Definition of done**. In some cases, it's not clear when to stop working on a feature. By following the steps, it's easy to verify that.

- **Empathy**. Verification steps are a journey, the same one users will embark on. By following the same path, it's easier to get in their shoes.

- **Keep the team in sync**. When everybody is working on their stuff, silos are born. By encouraging the team to test tickets via the verification steps, knowledge spreads.

- **Ensure successful deploys**. Ever `git push`ed and realized, hours later, it has been rejected because your local branch was behind? Or maybe forgot to toggle a feature flag? I cannot count the number of times I deployed a feature when I actually hadn't. Luckily, anybody can catch it by following the verification steps.

- **Build things right**. When focused on coding, developers embed their own bias into the software. A list of steps can not only shake programmers out of their own bubble, but they also allow the verifier to test according to their own bias. And yes, the more diverse the team, the better.

- **Build the right things**. It's challenging to *feel* a feature by reading a ticket description. Verification steps make it more apparent what the final user experience would be. Also, designers can make sure the steps represent what they envisioned.

- **Doing one thing at a time**. Any writer would say that drafting and editing don't go hand-in-hand. The same is valid for coding and manual testing. When the author of the code and the tester are the same person, verification steps help to enforce the boundaries.

- **Serendipitous verification**. I had clients, designers, and testers providing better technical solutions than whatever I came up with in the past. By enabling everybody to chime in with their expertise, bias, and freshness, better solutions surface.
