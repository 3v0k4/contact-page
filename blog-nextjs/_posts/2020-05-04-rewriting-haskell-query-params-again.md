---
title: Rewriting to Haskell–Parsing Query Params, Again
description: Wrapping URL query params in their own types without using FromHttpApiData
author: Riccardo
series: Rewriting to Haskell
tags:
  - Functional Programming
  - Haskell
  - Servant
---

In the [previous post](/posts/2020-04-27-rewriting-haskell-query-params/) we covered how to have Servant parse URL query parameters to custom data types. In this post, we see a similar technique without the use of `FromHttpApiData`.

The search endpoint in Stream has the following type signature:

```hs
type SearchAPI =
  QueryParam "query" Text
    :> QueryParam "quantity" Int
    :> QueryParam "comments" Bool
    :> QueryParam "channel" Text
    :> QueryParam "last_id" Int
    :> Get '[JSON] SearchResults
```

Which translates to the following handler function

```hs
getSearchResults
  :: Maybe Text
  -> Maybe Int
  -> Maybe Bool
  -> Maybe Text
  -> Maybe Int
  -> Handler SearchResults
```

Contrarily to the previous post, in this case we chose to use primitive types (e.g. `Text`, `Int`) instead of defining our own. What we do instead is to parse all the values in the first few lines of the handler:

```hs
getSearchResults configuration connection mQuery mQuantity mComments mChannel mLastId = do
  let searchQuery = mkSearchQuery mQuery
  let searchQuantity = mkSearchQuantity mQuantity
  let searchComments = mkSearchComments searchQuery mComments
  let searchChannels = mkSearchChannels mChannel
  let searchLastId = mkSearchLastId mLastId
  -- ...
```

By doing that, we can translate `Maybe`s into something that makes sense in Stream. For example, when in the URL `query` is not present or is an empty string, we want to return all posts. Otherwise, we use the value to filter:

```hs
data SearchQuery
  = Query Text
  | NoQuery

mkSearchQuery :: Maybe Text -> SearchQuery
mkSearchQuery Nothing = NoQuery
mkSearchQuery (Just "") = NoQuery
mkSearchQuery (Just query) = Query query
```

In the case of `quantity` (of posts returned) and (return posts older than) `last_id`:

```hs
data SearchQuantity
  = Limit Int
  | NoLimit

mkSearchQuantity :: Maybe Int -> SearchQuantity
mkSearchQuantity (Just quantity) = Limit quantity
mkSearchQuantity Nothing = NoLimit

data SearchLastId
  = LastId Int
  | NoLastId

mkSearchLastId :: Maybe Int -> SearchLastId
mkSearchLastId (Just lastId) = LastId lastId
mkSearchLastId Nothing = NoLastId
```

When it comes to what channels to search posts in, we limit to one only if `channel` is specified in the URL:

```hs
data SearchChannels
  = Channel Text
  | All

mkSearchChannels :: Maybe Text -> SearchChannels
mkSearchChannels Nothing = All
mkSearchChannels (Just channel) = Channel channel
```

More interesting is when the endpoint is instructed to also match against the comments belonging to the post:

```hs
data SearchComments
  = Enabled SearchQuery
  | Disabled

mkSearchComments :: SearchQuery -> Maybe Bool -> SearchComments
mkSearchComments searchQuery searchComments =
  case searchComments of
    Just True -> Enabled searchQuery
    _ -> Disabled
```

When `comments=true`, the query used to match against comments is the same used for posts. Otherwise, the search in comments is `Disabled`.
