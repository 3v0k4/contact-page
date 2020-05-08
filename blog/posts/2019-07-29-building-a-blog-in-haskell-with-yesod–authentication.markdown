---
title: Building a Blog in Haskell with Yesod–Authentication
author: Riccardo
description: In this post we are going to add authentication to our Yesod blog
cover_image: https://odone.io/images/yesod.png
tags:
  - Functional Programming
  - Haskell
  - Yesod
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

---

## Use `authDummy` for Logins

Up until now, we have used a form with username and password as a login. Turns out Yesod provides [`Yesod.Auth.Dummy`](http://hackage.haskell.org/package/yesod-auth-1.6.7/docs/Yesod-Auth-Dummy.html) to make development easy. Later on, in production [other providers](http://hackage.haskell.org/package/yesod-auth-1.6.7/docs/Yesod-Auth-Dummy.html) can be used.

![Screenshot of the Yesod dummy login form that consists of a text input and submit button](https://odone.io/images/dummy-login.png)

Commit [76c4347cdf4f563e9f543e83e5558848d29826a5](https://github.com/3v0k4/yesod-blog/commit/76c4347cdf4f563e9f543e83e5558848d29826a5):

- makes the landing page always redirect to the login page `redirect $ AuthR LoginR`
- sets the posts page as the target redirect for a successful login `loginDest _ = PostsR`
- sets our `emptyLayout` as a layout for the auth pages `authLayout = liftHandler . emptyLayout`


## Show Logged-in User in the Layout

Commit [aab5274c2ecb823b8c4d1c4eecfa37bf0e41c51f](https://github.com/3v0k4/yesod-blog/commit/aab5274c2ecb823b8c4d1c4eecfa37bf0e41c51f) adds the username of the logged-in user to the layout.

![Screenshot of the blog with the username of the current user at the top](https://odone.io/images/logged-in-user.png)

## Require Authentication for the Posts Page

Commit [9f97a457afdfb2c1fc7724dde990d650168783a7](https://github.com/3v0k4/yesod-blog/commit/9f97a457afdfb2c1fc7724dde990d650168783a7) adds authentication to the posts page:

```diff
- Nothing -> Unauthorized "You must login to access this page"
+ Nothing -> AuthenticationRequired
```

In other words, if an unauthenticated user tries to visit the posts page, a redirect to the login form is triggered.

## Tweaking the Landing

Commit [dd46b15777f6118074f4b2ce461f78c5efca8ef1](https://github.com/3v0k4/yesod-blog/commit/dd46b15777f6118074f4b2ce461f78c5efca8ef1) makes the landing page redirect to either the login page or the posts page depending on the authentication state of the current user.
