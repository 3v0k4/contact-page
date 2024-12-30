---
title: Scaffolding a Blog Post
description: Using a Haskell script to bootstrap a file from a template
author: Riccardo
tags:
  - Functional Programming
  - Haskell
  - Script
---

It's been two months since the [last post](/posts/2019-10-07-playing-with-fmt/).
I had a chance to learn a lot of cool stuff that I cannot wait to share! But let's start with something eazy.

Today, we are going to take a look at a short script written in Haskell with a simple goal in mind: scaffolding
a new file with an empty frontmatter, ready to house a new blog post (including this one!).

Hopefully, this will also show how nice it is to use Haskell even in a simple case.
Including how helpful is to have a strong type system holding our hands in the process.

We won't be covering the specifics of the
[Stack script interpreter](https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter). That has been
already covered in
["Scripting in Haskell and PureScript"](/posts/2019-07-08-scripting-in-haskell-and-purescript/).

Suffice to say we need to have an executable file (i.e. `chmod +x file.hs`) and the following at the top of
the file:

```hs
#!/usr/bin/env stack
{- stack
  script
  --resolver lts-14.17
  --package some-package
  --package some-other-package
-}
```

With that in place, `./file.hs` is enough to execute the script.

But let's dive right in the scaffolding one:

```hs
main :: IO ()
main = do
  today <- formatTime defaultTimeLocale "%F" <$> getCurrentTime
  --                                             ^ Get the current UTCTime from the system clock.
  --       ^ Format time to yyyy-mm-dd.
  let fileName = fold ["posts", "/", today, "-"]
  --             ^ Fold strings to a single one. `fold` does so by combining the elements of a
  --               structure using a monoid. String is an alias for [Char], the Monoid instance
  --               for [a] uses `<>` to combine `a`s together. In turn `<>` comes from Semigroup.
  --               The Semigroup instance for [a] defines `<>` as `++`.
  --               Therefore, `fileName` is the concatenation of the strings in the list.
  fileExist <- doesFileExist fileName
  when fileExist $ error "file already exists"
  --   ^ If it already exists..
  --               ^ ..stop execution and display an error message..
  writeFile fileName frontmatter
  -- ..otherwise write `frontmatter` to a file at path `fileName`.
```

And `frontmatter` is an empty frontmatter ready to be filled:

```hs
frontmatter :: String
frontmatter = unlines
  --          ^ An inverse operation to `lines`. It joins lines,
  --            after appending a terminating newline to each.
  [ "---"
  , "title:"
  , "description:"
  , "author:"
  , "cover_image:"
  , "tags:"
  , "  - FunctionalProgramming"
  , "---"
  ]
```

The whole script can be found on [Github](https://github.com/3v0k4/contact-page/blob/c1ff8925fdee946240fc97348946d18ef2ab7899/blog/scaffold.hs).
