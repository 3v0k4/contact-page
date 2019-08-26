---
title: Building a Blog in Haskell with Yesod–Giving Back
description: In this post I'm celebrating becoming a contributor to Yesod
author: Riccardo
cover_image: https://odone.io/images/yesod.png
---

This is a series about [Yesod](https://www.yesodweb.com/): a Haskell web framework that follows a similar philosophy to [Rails](https://rubyonrails.org/). In fact, it is strongly opinionated and provides a lot of functionality out of the box.

A good read about Yesod is available online for free: [Developing web applications with Haskell and Yesod](https://www.yesodweb.com/book). That's why this series will be a commentary of the commits from a [repo](https://github.com/3v0k4/yesod-blog) we will use to develop a super simple blog.

In other words, this won't be good material to learn how to use Yesod. However, it will hopefully give an overview of how the framework works.

Series index:

- [Building a Blog in Haskell with Yesod–The Basic Structure](https://odone.io/posts/2019-07-15-building-a-blog-in-haskell-with-yesod%E2%80%93the-basic-structure.html)
- [Building a Blog in Haskell with Yesod–Using a Database](https://odone.io/posts/2019-07-22-building-a-blog-in-haskell-with-yesod%E2%80%93using-a-database.html)
- [Building a Blog in Haskell with Yesod–Authentication](https://odone.io/posts/2019-07-29-building-a-blog-in-haskell-with-yesod%E2%80%93authentication.html)
- [Building a Blog in Haskell with Yesod–Authorization](https://odone.io/posts/2019-08-05-building-a-blog-in-haskell-with-yesod–authorization.html)
- [Building a Blog in Haskell with Yesod–Returning JSON](https://odone.io/posts/2019-08-12-building-a-blog-in-haskell-with-yesod–returning-JSON.html)
- [Building a Blog in Haskell with Yesod–JSON API](https://odone.io/posts/2019-08-19-building-a-blog-in-haskell-with-yesod–returning-JSON-API.html)
- [Building a Blog in Haskell with Yesod–Giving Back](https://odone.io/posts/2019-08-26-building-a-blog-in-haskell-with-yesod–giving-back.html)

## Giving Back

Last week has been a very special one. First of all, I had the chance of spending time with a couple of hundred [Software Crafters](https://www.softwarecrafters.org): the [SoCraTes conference](https://socrates-conference.de) has been the highlight of my year for the last three years. Also, helping with the organization is the way I try to give back to the community.

Something else happened last week that I'm really proud of. I got my first Pull Request merged for an open source project: I'm officially a [Haskell Yesod contributor](https://github.com/yesodweb/yesod/graphs/contributors)!!

The inspiration came from [last week's post](https://odone.io/posts/2019-08-19-building-a-blog-in-haskell-with-yesod–returning-JSON-API.html):

> Unfortunately, Yesod.Auth.Dummy does not support registration via JSON requests. Therefore, we have to patch it ourselves.

That's where I [asked](https://github.com/yesodweb/yesod/issues/1618) if a PR was of any interest to the maintainers, stared at the screen for several hours, copy / pasted some code from other parts of the codebase and submitted the new feature for review.

Special thanks to Michael, the author of Yesod, for the support and encouragement! I'm looking forward to the next Pull Request.
