---
title: Adding `published` to Hakyll posts
description: Control which posts to publish with a boolean flag in the metadata
published: false
author: Riccardo
tags:
  - FunctionalProgramming
  - Haskell
  - Hakyll
---

Last week I deployed the blog and published by mistake a post that I was keeping for the future:

<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Damn, I inadvertently published the post that was supposed to be out on Monday 😅</p>&mdash; Riccardo Odone (@RiccardoOdone) <a href="https://twitter.com/RiccardoOdone/status/1258722267338596354?ref_src=twsrc%5Etfw">May 8, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

I guess that was just a matter of time. I used to keep some unpublished posts around. Up until now, I was doing some hacky manual git stuff to schedule them. Not anymore!

I added support for an additional `published` field in the metadata of each post. The [scaffolding script](https://github.com/3v0k4/contact-page/blob/c4f8aca0cbe93d2acadb06bbff2eeb4c5dd91521/blog/scaffold.hs#L20) now adds `published: false` by default.

The core of the change resides in `site.hs`:

```diff
-  match "posts/*" $ do
+  matchMetadata "posts/*" isPublished $ do
```

In particular, instead of compiling all posts, it only takes the "published" ones. A post is considered published if

1. it does not have a `published` metadata field or
2. `published` is `true`.

I could have enforced all posts to have a `published` field and skip 1. but I was too lazy to retrofit that 😅

Here's the implementation:

```hs
isPublished :: Metadata -> Bool
isPublished = maybe True (== "true") . lookupString "published"
```

I actually [did one step more](https://github.com/3v0k4/contact-page/commit/c4f8aca0cbe93d2acadb06bbff2eeb4c5dd91521#diff-38bbc949e4276a9e92e6a46010c9b397) and decided to consider all posts published when a `HAKYLL_ENV` env variable is set to `development`:

```hs
env <- getEnvironment
hakyll $ do
  matchMetadata "posts/*" (isDevelopmentOrPublished env) $ do
-- ...

isDevelopmentOrPublished :: [(String, String)] -> Metadata -> Bool
isDevelopmentOrPublished env metadata = isDevelopmentEnv || isPublished
  where
    isDevelopmentEnv = lookup "HAKYLL_ENV" env == Just "development"
    isPublished = maybe True (== "true") . lookupString "published" $ metadata
```

That way, as long as I preview the blog with `HAKYLL_ENV=development stack exec site watch`, I can keep `published: false` while working on a new post.
