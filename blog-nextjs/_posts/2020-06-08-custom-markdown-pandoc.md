---
title: Custom Markdown in Pandoc
description: Transforming Pandoc's AST to implement custom Markdown syntax.
published: true
author: Riccardo
tags:
  - Functional Programming
  - Haskell
  - Hakyll
---

The architecture of Pandoc is well described in the [documentation](https://pandoc.org/filters.html):

> Pandoc consists of a set of readers and writers. When converting a document from one format to another, text is parsed by a reader into pandoc’s intermediate representation of the document—an “abstract syntax tree” or AST—which is then converted by the writer into the target format.

For example, let's take a Markdown file:

````bash
```haskell
x = 1
```
````

Pandoc translates it to the following AST:

```bash
stack install pandoc
stack exec -- pandoc -s -f markdown -t native file.markdown

# Pandoc
#   (Meta {unMeta = fromList []})
#   [CodeBlock ("",["haskell"],[]) "x = 1"]
```

`CodeBlock` is one of the value constructors of the `Block` type:

```hs
data Block
    = CodeBlock Attr Text
--  | ...

type Attr = (Text, [Text], [(Text, Text)])
--           ^ Id
--                 ^ Classes
--                         ^ Key-Value pairs
```

Pandoc allows changing the AST before the output document is written.

One way to achieve that is by using a filter:

```hs
#!/usr/bin/env runghc

{-# LANGUAGE OverloadedStrings #-}

import Text.Pandoc
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter transform

transform :: Block -> Block
transform (CodeBlock attr content) = CodeBlock attr "y = 2"
transform x = x
```

I'm using `runghc` in this case to make sure the version of Pandoc used in the filter is the same as the one invoked on the command line:

```bash
stack exec -- pandoc -s -f markdown -t native --filter filter.hs file.markdown

# Pandoc
#   (Meta {unMeta = fromList []})
#   [CodeBlock ("",["haskell"],[]) "y = 2"]
```

Notice that the code changed from `x = 1` to `y = 2`.

The same can be achieved in pure Haskell code:

```hs
{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Text.Pandoc
import Text.Pandoc.JSON
import Text.Pandoc.Walk

main :: IO ()
main = do
  file <- readFile "./file.markdown"
  result <- runIO $ do
    doc <- readMarkdown (def {readerExtensions = pandocExtensions}) (pack file)
    let transformed = walk transform doc
    writeNative (def {writerExtensions = pandocExtensions}) transformed
  handleError result >>= putStrLn . unpack

transform :: Block -> Block
transform (CodeBlock attr content) = CodeBlock attr "y = 2"
transform x = x
```

```bash
stack runghc pure-haskell.hs

# [CodeBlock ("",["haskell"],[]) "y = 2"]
```

With Hakyll, similar transformations can be achieved by using `pandocCompilerWithTransform` or `pandocCompilerWithTransformM`.

That is how tweetable pull quotes are [implemented](https://github.com/3v0k4/contact-page/commit/539d89253879e903f3350e187885e4ac1f72a165) on this blog. In particular, the following Markdown

````md
```pullquote
If we are not open to silly ideas, then why even bother with a creative activity in the first place?
```

In a recent workshop I attended one rule was "all ideas are brilliant". Yes, at first an idea could be raw, maybe even silly. However, by thinking outside the box, it may turn into something innovative. More often than not though, it will be a draw in the blank. Still, if we are not open to silly ideas, then why even bother with a creative activity in the first place? In fact, if you always do what you've always done, you'll always get what you've always got.
````

becomes

[![Screenshot of a pull quote from a blog post](/images/pull-quote.png)](/posts/2020-05-29-silly-questions/)

---

If you want to dig deeper, the [Pandoc documentation on filters](https://pandoc.org/filters.html) is a great read on the topic.
