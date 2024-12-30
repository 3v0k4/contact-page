---
title: Building a Blog in Haskell with Yesod–Using a Database
author: Riccardo
description: In this post we are going to hook up our Yesod blog to a database
cover_image: /images/yesod.png
series: Building a Blog in Haskell with Yesod
tags:
  - Functional Programming
  - Haskell
  - Yesod
---

This is a series about [Yesod](https://www.yesodweb.com/): a Haskell web framework that follows a similar philosophy to [Rails](https://rubyonrails.org/). In fact, it is strongly opinionated and provides a lot of functionality out of the box.

A good read about Yesod is available online for free: [Developing web applications with Haskell and Yesod](https://www.yesodweb.com/book). That's why this series will be a commentary of the commits from a [repo](https://github.com/3v0k4/yesod-blog) we will use to develop a super simple blog.

In other words, this won't be good material to learn how to use Yesod. However, it will hopefully give an overview of how the framework works.

---

## Interacting with SQLite3

Before we start, let's review a few commands to interact with SQLite3. That is the database we are using because it's the default one for scaffolded Yesod apps.

- "open" a database: `SQLite3 MY_DATABASE.sqlite3`
- list all tables: `.tables`
- check the schema of a table `.schema MY_TABLE`

Later on in the post it could be useful to create a `user` or a `post`:

```sql
INSERT INTO user (ident, password) VALUES ("admin", "admin");
INSERT INTO post (title, text) VALUES ("title", "text");
```

## Login User Only if in Database

Commit [b90b5cc29b053ed900a8b395b097688264388ebf](https://github.com/3v0k4/yesod-blog/commit/b90b5cc29b053ed900a8b395b097688264388ebf) introduces a conditional when the user submits the login form. In particular, it checks if a user with the submitted username and password is present in the database. If that's the case, the user is redirected to the posts page. Otherwise, the login form is re-rendered.

In this case, we've reused the Persist entity that was given to us by the scaffolded Yesod app:

```bash
User
    ident Text
    password Text Maybe
    UniqueUser ident
```

We prolly should have changed `ident` to `username` and `password` to be `Text` and not `Maybe Text`. Since other scaffolded code relies on that definition and we may want to reuse that in the future, we kept it as it was.

The important part of the commit is

```hs
user <- runDB $ selectList [ UserIdent ==. username login, UserPassword ==. Just (password login) ] []
```

The [`selectList`](https://hackage.haskell.org/package/persistent-2.10.0/docs/Database-Persist-Class.html#v:selectList) is from Persistent. The type looks scary but in this case it simply returns a list of users which match `username` and `password` from the login form.

The above-implementation is not at all useful nor secure. In fact, no session for the user is created and the password is assumed to be saved in the database in clear. We will fix the session part in a later post.

## Make Post an Entity

Commit [525368882062eeeb1717e7112c74b25214fdf736](https://github.com/3v0k4/yesod-blog/commit/525368882062eeeb1717e7112c74b25214fdf736) makes the `Post` data type a Persist entity. By doing that we get generated for free:

- the `Post` data type that was just removed
- other bits and pieces we will need to interact with posts and the database

## Posts from Database

Commit [7a0735fc2fe8ce3420773cd2bd8a41dde6d74fa3](https://github.com/3v0k4/yesod-blog/commit/7a0735fc2fe8ce3420773cd2bd8a41dde6d74fa3) removes the hardcoded posts in favour of the ones present in the database.

The important part is

```hs
allPosts :: [Entity Post] <- runDB $ selectList [] []
```

The type annotation is needed to let Persistent know against what table to run `selectList`.

## Posts Ordering

Commit [716817e7b54310fd1b588e4a77db5d199e084383](https://github.com/3v0k4/yesod-blog/commit/716817e7b54310fd1b588e4a77db5d199e084383) makes sure the posts are ordered in `Desc`ending id order:

```hs
allPosts :: [Entity Post] <- runDB $ selectList [] [ Desc PostId ]
```

## Create Post in Database

Commit [1ec890d790f08027fc5e58bfa8d3a354d86ab4e0](https://github.com/3v0k4/yesod-blog/commit/1ec890d790f08027fc5e58bfa8d3a354d86ab4e0) connects the new post form to the database. In other words, whenever the form is submitted successfully, a new record is added to the database.

## Delete Post Button

Commit [4fea7c6460f73615d615ba03e46e157c4e949424](https://github.com/3v0k4/yesod-blog/commit/4fea7c6460f73615d615ba03e46e157c4e949424) adds a delete button to each post.

HTML `form`s only allow to use `GET` or `POST` as HTTP actions. To follow REST we need a `DELETE` in this case. We can simulate that with

```hs
<form method=post action=@{PostR $ entityKey post}?_method=DELETE>
```

The key part is `?_method=DELETE`. With that url parameter, the form will be routed to `deletePostR` instead of `postPostR`. That happens thanks to [Network.Wai.Middleware.MethodOverride](https://stackoverflow.com/questions/22902419/yesod-put-and-delete-using-hidden-method-parameter/22903897#answer-22903897).
