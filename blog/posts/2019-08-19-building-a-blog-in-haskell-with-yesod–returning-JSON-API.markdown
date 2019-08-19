---
title: Building a Blog in Haskell with Yesod–JSON API
description: In this post we are going to see how to transform our Yesod blog into a JSON API (including authentication).
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
- Building a Blog in Haskell with Yesod–JSON API (this post)

## Back in Business

[Last week's post](https://odone.io/posts/2019-08-12-building-a-blog-in-haskell-with-yesod–returning-JSON.html) started on a bitter note:

> The plan for this post was to transform the entire blog into an API. Unfortunately, the compiler got in the middle.

It's not a secret that the Haskell compiler can be at times difficult to satisfy. However, what a joy it is when the program finally type checks! This time we got it covered. So let's dive into it.

## Authentication

Last week's post was a bit of a lie. In fact, since the authentication wasn't taken care of, the user had to adhere to the following steps to get the list of posts in JSON format:

- visit `/api/posts` on a browser
- the application would have shown the login form
- submit the login form
- visit `/api/posts` on a browser again

That is because the user didn't have any means to authenticate the `/api/posts` requests. Therefore, they had to create a session (cookie) by logging in.

Commit [cd78427e82babef42f170bf7b3e4ff423d88a729](https://github.com/3v0k4/yesod-blog/commit/cd78427e82babef42f170bf7b3e4ff423d88a729) fixes that by patching `maybeAuthId`:

```hs
    maybeAuthId = do
        request <- waiRequest
        let mHeader = lookup "X-User-Id" (Network.Wai.requestHeaders request)
            bsToText = T.pack . BSC8.unpack
        case bsToText <$> mHeader of
          Just v ->
            return $ fromPathPiece v
          Nothing ->
            defaultMaybeAuthId
```

The default [`maybeAuthId`](http://hackage.haskell.org/package/yesod-auth-1.6.7/docs/Yesod-Auth.html#v:maybeAuthId)

> Retrieves user credentials, if user is authenticated.
>
> By default, this calls defaultMaybeAuthId to get the user ID from the session.

The docs go on saying

> This can be overridden to allow authentication via other means, such as checking for a special token in a request header. This is especially useful for creating an API to be accessed via some means other than a browser.

That is why, the patched code above looks for the id in the `X-User-Id` header and delegates to the default behaviour when not found.

## Registration

Up until now we've been using [`Yesod.Auth.Dummy`](http://hackage.haskell.org/package/yesod-auth-1.6.7/docs/Yesod-Auth-Dummy.html) for both authentication and registration. In fact, `Yesod.Auth.Dummy` renders a login form with one text field. If it's submitted with the username of an existing user, then that becomes the logged in user. Otherwise, it first adds a new record to the database and then creates the session.

Unfortunately, `Yesod.Auth.Dummy` does not support registration via JSON requests. Therefore, we have to patch it ourselves.

Commit [78e13f807f5674aa0a84e2633d7967fa02b755cf](https://github.com/3v0k4/yesod-blog/commit/78e13f807f5674aa0a84e2633d7967fa02b755cf) just copy / pastes the `authDummy` code from `Yesod.Auth.Dummy`. The real change is done in commit [cd78427e82babef42f170bf7b3e4ff423d88a729](https://github.com/3v0k4/yesod-blog/commit/cd78427e82babef42f170bf7b3e4ff423d88a729).

Firstly, we introduce a parser 

```hs
parser :: Value -> Parser Text
parser = withObject "ident" (\obj -> do
                ident <- obj .: "ident"
                return ident)
```
that is capable of extracting `ident` from a JSON like the following

```json
{ "ident": "MY USERNAME" }
```

Secondly, we patch the `dispatch` function of the `authDummy` plugin. That is the place in charge of taking care of the POST requests to `/auth/page/dummy`. Up until now, it only worked when the html form was submitted. To make it work with a JSON request we need to make a few changes:

```diff
 authDummy :: YesodAuth m => AuthPlugin m
 authDummy =
     AuthPlugin "dummy" dispatch login
   where
     dispatch "POST" [] = do
-        ident <- runInputPost $ ireq textField "ident"
-        setCredsRedirect $ Creds "dummy" ident []
+        result <- runInputPostResult $ ireq textField "ident"
+        case result of
+          FormSuccess ident ->
+            setCredsRedirect $ Creds "dummy" ident []
+          _ -> do
+            (result :: Result Value) <- parseCheckJsonBody
+            case result of
+              Success val -> do
+                let mIdent = parseEither parser val
+                case mIdent of
+                  Right ident ->
+                    setCredsRedirect $ Creds "dummy" ident []
+                  Left err ->
+                    invalidArgs [T.pack err]
+              Error err ->
+                invalidArgs [T.pack err]
     dispatch _ _ = notFound
```

In more detail, we first try to extract `ident` from the POST params (i.e. form submission) with `runInputPostResult`. If that succeeds (i.e. `FormSuccess`) we leave the original behaviour intact.

If it fails, we try to get the JSON body with `parseCheckJsonBody`. If that fails we return `invalidArgs` (i.e. HTTP 400). If it succeeds, we try to extract from the JSON body the `ident` Text. If that succeeds we do the same thing as when the POST params succeed.

## Create a New Post

It turns out, many concepts used in the previous section apply to creating a new post. We do that in commit [4922c8db2706fd0999ef478373b82326f5851b4d](https://github.com/3v0k4/yesod-blog/commit/4922c8db2706fd0999ef478373b82326f5851b4d):

```hs
postApiPostsR :: Handler Value
postApiPostsR = do
  (result :: Result Value) <- parseCheckJsonBody
  case result of
    Success val -> do
      let mPost = parseEither postParser val
      case mPost of
        Right post -> do
          postId <- runDB $ insert post
          return $ object [ "post" .= post, "id" .= postId ]
        Left err ->
          invalidArgs [pack err]
    Error err ->
      invalidArgs [pack err]

postParser :: Value -> Parser Post
postParser = withObject "Post" (\obj -> do
                title <- obj .: "title"
                text <- obj .: "text"
                userId <- obj .: "userId"
                return $ Post title text userId)
```

## Delete a Post

This is the easiest part, look how small commit [166aa28741fe532ae664308a2af4b28638b6d560](https://github.com/3v0k4/yesod-blog/commit/166aa28741fe532ae664308a2af4b28638b6d560) is!

The only thing worth mentioning is the `return Null` in

```hs
deleteApiPostR :: PostId -> Handler Value
deleteApiPostR postId = do
  _ <- runDB $ delete postId
  return Null
```

That is just a shortcut to return an empty JSON body.

## CURLing

Register:

```bash
curl -XPOST -d '{"ident":"super cool username"}' -H "Content-Type: application/json" -H "Accept: application/json" http://localhost:3000/auth/page/dummy

# {"message":"Login Successful"}
```

Un-authenticated request:

```bash
curl -H "Accept: application/json" http://localhost:3000/api/posts

# {"authentication_url":"http://localhost:3000/auth/login","message":"Not logged in"}
```

List of posts:

```bash
curl -H "X-User-Id: 1" -H "Accept: application/json" http://localhost:3000/api/posts

# {"posts":[{"text":"Luigi","user": {"username":"luigi","id":3},"id":5,"title":"I am"},{"text":"333","user":{"username":"mario","id":2},"id":4,"title":"333"},{"text":"text","user":{"username":"mario","id":2},"id":3,"title":"title"},{"text":"text","user"{"username":"riccardo","id":1},"id":2,"title":"title"},{"text":"1","user":{"username":"riccardo","id":1},"id":1,"title":"1"}]}
```

Create a new post with missing parameter:

```bash
curl -XPOST -d '{"title":"some title","text":"some text"}' -H "X-User-Id: 1" -H "Content-Type: application/json" -H "Accept: application/json" http://localhost:3000/api/posts

# {"message":"Invalid Arguments","errors":["Error in $: key \"userId\" not present"]}
```

Create a new post:

```bash
curl -XPOST -d '{"title":"some title","text":"some text","userId": 1}' -H "X-User-Id: 1" -H "Content-Type: application/json" -H "Accept: application/json" http://localhost:3000/api/posts

# {"post":{"text":"some text","userId":1,"title":"some title"},"id":6}
```

Delete a post when not owner:

```bash
curl -XDELETE -H "X-User-Id: 2" -H "Accept: application/json" http://localhost:3000/api/posts/6

# {"message":"Permission Denied. only the author can delete their post"}
```

Delete a post:

```bash
curl -XDELETE -H "X-User-Id: 1" -H "Accept: application/json" http://localhost:3000/api/posts/6

# null
```


## Outro

Here we have our JSON API. This has been quite a struggle. But what a great feeling when it finally compiled!!!
