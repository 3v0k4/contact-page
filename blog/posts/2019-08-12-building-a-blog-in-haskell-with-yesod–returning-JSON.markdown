---
title: Building a Blog in Haskell with Yesod–Returning JSON
description: In this post we are going to see how to return JSON in our Yesod blog
author: Riccardo
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

## Gimme JSON!

The plan for this post was to transform the entire blog into an API. Unfortunately, the compiler got in the middle. Therefore, this is just the first step towards that goal.

In particular, here we are going to see how to return the list of posts as JSON instead of as HTML.

Commit [53c06240c8b41438f35475284588e67950ff8800](https://github.com/3v0k4/yesod-blog/commit/53c06240c8b41438f35475284588e67950ff8800) is the initial attempt. 

By adding `json` to the `Post` and `User` model, we get for free a `ToJSON` instance. Then, we can use it in the handler with

```hs
return $ object [ "posts" .= allPosts ]
```

Notice that `allPosts` is a list of tuples. In particular, `(post, user)`. That's why the final JSON looks like the following:

```json
{
  "posts":[
    [
      {
        "text":"Luigi",
        "userId":3,
        "id":5,
        "title":"I am"
      },
      {
        "password":null,
        "ident":"luigi",
        "id":3
      }
    ],
    [
      {
        "text":"333",
        "userId":2,
        "id":4,
        "title":"333"
      },
      {
        "password":null,
        "ident":"mario",
        "id":2
      }
    ],
    [
      {
        "text":"text",
        "userId":2,
        "id":3,
        "title":"title"
      },
      {
        "password":null,
        "ident":"mario",
        "id":2
      }
    ],
    [
      {
        "text":"text",
        "userId":1,
        "id":2,
        "title":"title"
      },
      {
        "password":null,
        "ident":"riccardo",
        "id":1
      }
    ],
    [
      {
        "text":"1",
        "userId":1,
        "id":1,
        "title":"1"
      },
      {
        "password":null,
        "ident":"riccardo",
        "id":1
      }
    ]
  ]
}
```

## Custom JSON

It's cool to have the `ToJSON` instances generated for free. Unfortunately, that means we don't have any control over the content of the JSON.

Commit [70e71484dd979fe43adf6295f4f6110e974a3214](https://github.com/3v0k4/yesod-blog/commit/70e71484dd979fe43adf6295f4f6110e974a3214) fixes that by wrapping the `(post, user)` tuple into a new datatype with its own `ToJSON` instance:

```hs
instance ToJSON PostData where
  toJSON (PostData (postEntity, userEntity)) =
    let
      post = entityVal postEntity
      postId = entityKey postEntity
      user = entityVal userEntity
      userId = entityKey userEntity
    in
    object
      [ "id" .= postId
      , "title" .= postTitle post
      , "text" .= postText post
      , "user" .= object
        [ "id" .= userId
        , "username" .= userIdent user
        ]
      ]
```

That means our JSON now looks like this

```json
{
  "posts":[
    {
      "text":"Luigi",
      "user":{
        "username":"luigi",
        "id":3
      },
      "id":5,
      "title":"I am"
    },
    {
      "text":"333",
      "user":{
        "username":"mario",
        "id":2
      },
      "id":4,
      "title":"333"
    },
    {
      "text":"text",
      "user":{
        "username":"mario",
        "id":2
      },
      "id":3,
      "title":"title"
    },
    {
      "text":"text",
      "user":{
        "username":"riccardo",
        "id":1
      },
      "id":2,
      "title":"title"
    },
    {
      "text":"1",
      "user":{
        "username":"riccardo",
        "id":1
      },
      "id":1,
      "title":"1"
    }
  ]
}
```
