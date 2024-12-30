---
title: Production Drafts for Hakyll Posts
description: Publish hidden drafts to production with a boolean flag in the metadata
published: true
author: Riccardo
tags:
  - Functional Programming
  - Haskell
  - Hakyll
---

Last week I was asked to review a draft of a blog post:

<blockquote class="twitter-tweet"><p lang="en" dir="ltr"><a href="https://twitter.com/RiccardoOdone?ref_src=twsrc%5Etfw">@RiccardoOdone</a> I just found your blog and am really digging your Haskell content. I&#39;m working on a blog explaining property based testing options, could I ask you to take a look at it? I&#39;d love your feedback / opinion :)</p>&mdash; Maxfield Chen (@_nihliphobe) <a href="https://twitter.com/_nihliphobe/status/1260669421099446272?ref_src=twsrc%5Etfw">May 13, 2020</a></blockquote>

That made me realize I don't have a way to do a similar trick. However, last week I worked on ["Adding `published` to Hakyll Posts"](/posts/2020-05-18-published-posts-hakyll/). Thus, I already did the hard work on understanding the internals, so I could just sit back and code it.

Up until now the blog worked as follows:

- Published posts were compiled by Hakyll to `posts/*`.
- Drafts were not compiled except when `HAKYLL_ENV` was set to `development`.

With the [new changes](https://github.com/3v0k4/contact-page/commit/a5c435a2177f7ca3d73622986765f57e87c62085):

- All posts are compiled by Hakyll.
- Published posts are compiled to `posts/*` and appear in the [archive](/archive/).
- Drafts are compiled to `drafts/posts/*` but do not appear in the [archive](/archive/).

In particular, only published posts are compiled to `posts/*`:

```diff
- matchMetadata "posts/*" (isDevelopmentOrPublished env) $ do
+ matchMetadata "posts/*" isPublished $ do
    route $ setExtension "html"
```

Drafts are compiled to `drafts/posts/*`. Also, when compiling the URL to the draft is printed:

```hs
matchMetadata "posts/*" (not . isPublished) $ do
  let draftPath = ("drafts/" <>) . (`replaceExtension` "html") . toFilePath
  route . customRoute $ draftPath
  let putDraftUrl path =
        traverse_
          (unsafeCompiler . putStrLn)
          [ "----DRAFT----",
            (previewUrl <>) . draftPath . itemIdentifier $ path,
            "-------------"
          ]
  compile $
    pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls
      >>= (\x -> putDraftUrl x >> pure x)
```

In the archive only published posts are included except when in development:

```diff
  create ["archive.html"] $ do
    route idRoute
    compile $ do
-     posts <- recentFirst =<< loadAll "posts/*"
+     posts <- recentFirst =<< loadAllPublished env "posts/*"
```

```hs
loadAllPublished :: (Binary a, Typeable a) => [(String, String)] -> Pattern -> Compiler [Item a]
loadAllPublished env pattern_ = if isDevelopmentEnv env then all else published
  where
    all = loadAll pattern_
    published = publishedIds pattern_ >>= traverse load
    isDevelopmentEnv env = lookup "HAKYLL_ENV" env == Just "development"
```

A similar change appears in the Atom feed code:

```diff
- =<< loadAllSnapshots "posts/*" "content"
+ =<< loadAllSnapshotsPublished "posts/*" "content"
```

```hs
loadAllSnapshotsPublished :: (Binary a, Typeable a) => Pattern -> Snapshot -> Compiler [Item a]
loadAllSnapshotsPublished pattern_ snapshot = publishedIds pattern_ >>= traverse (`loadSnapshot` snapshot)

publishedIds :: MonadMetadata m => Pattern -> m [Identifier]
publishedIds = fmap (fmap fst . filter (isPublished . snd)) . getAllMetadata
```

As always, feel free to go ahead and grab the [code](https://github.com/3v0k4/contact-page/blob/a5c435a2177f7ca3d73622986765f57e87c62085/blog/site.hs).

---

The draft I ended up reviewing was ["Functional Fika — Nix and Haskell"](https://maxfieldchen.com/posts/2020-05-16-Functional-Fika-Haskell-Nix-Cabal.html). Thanks Maxfield for the inspiration and for teaching me what fika is! ☕️
