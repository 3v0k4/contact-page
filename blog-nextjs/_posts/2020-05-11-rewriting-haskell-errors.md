---
title: Rewriting to Haskell–Errors
description: Returning the correct JSON error when parsing fails
author: Riccardo
series: Rewriting to Haskell
tags:
  - Functional Programming
  - Haskell
  - Servant
---

The frontend code for Stream expects errors to have the following shape:

```json
{
  "errors": {
      "text": [ "first error", "second error", ... ],
      ...
  },
  ...
}
```

For example when the `text` for a comment is empty the response should report that:

```json
{ "errors": { "text": [ "can't be blank" ] } }
```

That happens for example when the request body to create a new comment is something like:

```json
{ "post_id": 1 }
```

In fact, as shown below, Servant tries to parse the request body into a value of type `CommentRequest` but fails because `text` is missing:

```hs
type CommentsAPI = ReqBody '[JSON] CommentRequest :> Servant.Post '[JSON] Response

data CommentRequest =
  CommentRequest
    { commentRequestPostId :: Int
    , commentRequestText :: Text
    } deriving stock (Eq, Show)

instance FromJSON CommentRequest where
  parseJSON = withObject "CommentRequest" $ \v ->
    CommentRequest
      <$> v .: "post_id"
      <*> v .: "text"
```

Servant, when a parsing error happens, returns a 400 without a `Content-Type` and a simple string:

```md
Error in $: key "text" not found
```

That is not what we want! The really cool thing is that, since Elm is [using all the decoders magic](https://guide.elm-lang.org/effects/json.html), stuff still works! In particular, "An error occurred while submitting the comment" is shown at the top of the page and the application does not crash. I'm pretty sure I would have screwed it up in a language where error handling is optional.

However, had the proper error been returned, Elm would know what attribute was bad. For example, with

```json
{ "errors": { "text": [ "can't be blank" ] } }
```

The frontend would add an error below the field in the form:

![Screenshot of the comment form in Stream with a red error message right below the text field](https://odone.io/images/elm-comment-error.png)

Therefore, we need to make sure that if in a body of a POST some key/value pair is missing, we return the correct json response. In other words, instead of the following:

```md
Error in $: key "text" not found
```

We need to return:

```json
{ "errors": { "text": [ "some error here" ] } }
```

with a `Content-Type: application/json` header.

Luckily, [servant-errors](https://github.com/epicallan/servant-errors) has our back! The readme and associated [blog post](https://lukwagoallan.com/posts/unifying-servant-server-error-responses) explain it pretty well.

In Stream we needed a couple of changes:

```diff
 app :: Configuration -> Connection -> Application
-app configuration connection = serve api $ server configuration connection
+app configuration connection = errorMwJson (serve api $ server configuration connection)
```

With `errorMwJson` defined as follows:

```hs
errorMwJson :: Application -> Application
errorMwJson = errorMw @(Ctyp JSON) @'[]

data Ctyp a deriving (Accept) via JSON

instance HasErrorBody (Ctyp JSON) '[] where
  encodeError = encodeAsJsonError

encodeAsJsonError :: StatusCode -> ErrorMsg -> ByteString
encodeAsJsonError _ content =
  encode . HashMap.fromList $ [("errors" :: Text, formatErrors . unErrorMsg $ content)]

formatErrors :: Text -> Value
formatErrors error_ = case parse aesonNotFoundKey "" error_ of
  Right field -> toJSON . HashMap.fromList $ [(field, ["missing in request body" :: Text])]
  Left _ -> toJSON error_

-- Parses things like `Error in $: key \"text\" not found`
aesonNotFoundKey :: Stream s m Char => ParsecT s u m Text
aesonNotFoundKey = do
  _ <- string "Error in $"
  _ <- manyTill anyChar $ char ':'
  _ <- string " key \""
  field <- T.pack <$> many1 letter
  _ <- string "\" "
  _ <- string "not found"
  pure field
```

Here's the test:

```hs
it "with missing text in the request body it returns a json response with errors.test" $ do
  currentUser <- liftIO $ createUser connection
  slackToken <- liftIO randomText
  let url = "/servant/comments?slack_token=" <> encodeUtf8 slackToken <> "&user_id=" <> (BS.pack . show . userId $ currentUser)
  let postAttributes = defaultPostAttributes {postAttributesUserId = userId currentUser}
  post_ <- liftIO $ createPost connection postAttributes
  let body = [json| { post_id: #{postId post_} } |]
  post url body `shouldRespondWith` [json| { errors: { text: ["missing in request body"]} } |] {matchStatus = 400}
```

Had we been using [some](https://hackage.haskell.org/package/servant-elm) Servant mechanism to derive Elm functions to query the endpoint, this would have been taken care of automatically. In fact, it would be impossible to send a wrong request (i.e. Elm would not compile). However, it was surprising to discover this default behaviour. Servant still rocks hard though! 🤘

Thanks Allan for authoring [servant-errors](https://github.com/epicallan/servant-errors)! Wow, that saved me so many headaches!!
