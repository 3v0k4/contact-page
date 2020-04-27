---
title: Rewriting to Haskell–Parsing Query Params
description: Wrapping URL query params in their own types using FromHttpApiData
author: Riccardo
tags:
  - FunctionalProgramming
  - Haskell
  - Servant
---

This is part of a series:

- [Rewriting to Haskell–Intro](https://odone.io/posts/2020-02-26-rewriting-haskell-intro.html)
- [Rewriting to Haskell–Project Setup](https://odone.io/posts/2020-03-03-rewriting-haskell-setup.html)
- [Rewriting to Haskell–Deployment](https://odone.io/posts/2020-03-14-rewriting-haskell-server.html)
- [Rewriting to Haskell–Automatic Formatting](https://odone.io/posts/2020-03-19-rewriting-haskell-formatting.html)
- [Rewriting to Haskell–Configuration](https://odone.io/posts/2020-03-23-rewriting-haskell-configuration.html)
- [Rewriting to Haskell–Standing on the shoulders of Rails](https://odone.io/posts/2020-03-30-rails.html)
- [Rewriting to Haskell–Making GHC More Nitpicky](https://odone.io/posts/2020-04-06-ghc-options.html)
- [Rewriting to Haskell–Testing](https://odone.io/posts/2020-04-13-rewriting-haskell-testing.html)
- [Rewriting to Haskell–Linting](https://odone.io/posts/2020-04-13-rewriting-haskell-linting.html)

---

As discussed in the [series intro](https://odone.io/posts/2020-02-26-rewriting-haskell-intro.html), we are rewriting Stream from Rails to Servant. Since we value small iterations, the idea is to rely as much as possible on existing code while migrating Ruby to Haskell. For that reason, we decided to move endpoint by endpoint and leave the authentication in Rails for the time being.

Stream authenticates users via Slack OAuth. In other words, Rails, for any given authenticated request, knows the `slack_token` of the current user. We started with the following endpoint in Servant:

```hs
type CommentsAPI =
-- ^ Type definition for the comments Api.
-- ^ For now we just expose one endpoint to create a new comment.
  QueryParam "slack_token" Text
-- ^ Expect a query param..
--           ^ ..named `slack_token`..
--                         ^ ..and parse it as `Text`.
    :> ReqBody '[JSON] CommentRequest
--     ^ The request body..
--             ^ ..will be JSON..
--                     ^ ..parsed to a value of type `CommentRequest`.
    :> Post '[JSON] Response
--     ^ The endpoint is exposed as a POST..
--          ^ ..it will return a JSON representation..
--                  ^ ..of a value of type `Response`.
```

With the declaration above, the handler function would be something like:

```hs
postComment
  :: Maybe Text
  -> CommentRequest
  -> Handler Response
```

Notice that `slack_token` is parsed as `Maybe Text` not `Text`. In fact, being a query parameter, Servant takes care of the fact that it could be missing.

Since we are passing `slack_token` from Rails, we are confident it will always be there. For that reason, we let Servant know so that we do not need to deal with a `Maybe` (the [docs](https://hackage.haskell.org/package/servant-0.17/docs/Servant-API.html#t:QueryParams) are pretty clear on the details):

```diff
- QueryParam "slack_token" Text
+ QueryParam' '[Required, Strict] "slack_token" Text

  postComment
-   :: Maybe Text
+   :: Text
    -> CommentRequest
    -> Handler Response
```

Let's go one step deeper into the rabbit hole.

Servant parses values to the specified types. For example, we declared that `slack_token` will be parsed as `Text`. But how does the framework know how to do that? Simple, any value of a type with an instance of [`FromHttpApiData`](https://hackage.haskell.org/package/http-api-data-0.4.1.1/docs/Web-HttpApiData.html#t:FromHttpApiData) can be parsed. `Text`, implements it, thus we can use it out of the box.

That means we can ask Servant to parse `slack_token` as a value of type `SlackToken`:

```diff
+ newtype SlackToken = SlackToken Text

+ instance FromHttpApiData SlackToken where
+   parseUrlPiece = Right . SlackToken

- QueryParam' '[Required, Strict] "slack_token" Text
+ QueryParam' '[Required, Strict] "slack_token" SlackToken

  postComment
-   :: Maybe Text
+   :: SlackToken
    -> CommentRequest
    -> Handler Response
```

Now, not only we are not passing an anonymous `Text` around, also we could add some validation logic to `parseUrlPiece` and return a `Left error` in case that failed. In the latter case, Servant would return `error` without invoking the handler.
