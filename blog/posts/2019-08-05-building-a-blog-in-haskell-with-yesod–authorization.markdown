---
title: Building a Blog in Haskell with Yesod–Authorization
author: Riccardo
description: In this post we are going to take care of authorization in our Yesod blog
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

## Who's the Author?

Up until now, the logged-in user didn't have any relationships with the posts. Commit [b9ed6789ed578e4349f9fc0eee670e2df87434be](https://github.com/3v0k4/yesod-blog/commit/b9ed6789ed578e4349f9fc0eee670e2df87434be) adds a `userId` to `Post` and makes sure it gets filled with the id of the authenticated user.

## Authorize Deletions

In a multi-author blog, only the owner should be allowed to delete a post. Commit [db722e785cc09ad5642486df17c770e85899648c](https://github.com/3v0k4/yesod-blog/commit/db722e785cc09ad5642486df17c770e85899648c) takes care of that. The important bit is the following

```hs
isAuthorized (PostR postId) _ = isOwner postId
```

## Delete Button

Since only the owner can delete a post, it makes sense to reflect that in the UI. Commit [2378194354b6e0e92fb1c83ac5feb97aac8d219b](https://github.com/3v0k4/yesod-blog/commit/2378194354b6e0e92fb1c83ac5feb97aac8d219b) does exactly that:

```hs
$if userId == (postUserId $ entityVal post)
  <button>Delete
$else
  <p>
```

## Show me the Author!

The last thing to do is to show the author names alongside their blogposts. Given our database schema (`config/models.persistentmodels`)

```bash
User
    ident Text
    password Text Maybe
Post
    title Text
    text Textarea
    userId UserId
```

we need to perform a join between `user` and `post`.

Unfortunately, the default database library for Yesod, Persistent, doesn't support joins in a type-safe way. In fact, the only way would be to use [`rawSql`](https://hackage.haskell.org/package/persistent-2.10.0/docs/Database-Persist-Sql.html#v:rawSql).

Luckily, we can easily add [Esqueleto](http://hackage.haskell.org/package/esqueleto) which builds on top of Persistent and is capable of performing type-safe joins: [78ef59c6e6718dbce83ea2802cb70335bb4cca33](https://github.com/3v0k4/yesod-blog/commit/78ef59c6e6718dbce83ea2802cb70335bb4cca33)

## Screenshot or didn't Happen!

Here we can see that the delete button is shown only to the owner of the post and that the author names is displayed together with title and text:

![Screenshot of the blog with two posts where the current user can only delete their post](https://odone.io/images/authorization.png)
