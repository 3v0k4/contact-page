---
title: Adding Twitter Cards to Hakyll Posts
author: Riccardo
description: Make your Hakyll posts look better in Twitter
cover_image: https://odone.io/images/card.jpg
tags:
  - Functional Programming
  - Haskell
  - Hakyll
---

## Intro

By default Hakyll does not add the proper meta tags for Twitter Cards. For that reason, if we try to tweet a link to a Hakyll post, no card will be rendered. We can confirm that by using [Twitter's Card Validator](https://cards-dev.twitter.com/validator):

![Twitter card validator saying "Unable to render card preview"](https://odone.io/images/card-no-metas.png){.content-image}

That can be fixed with a few lines of code:

![Twitter card validator showing the Twitter card with post's cover_image, title and description](https://odone.io/images/card-with-metas.png){.content-image}

## The Few Lines of Code

As explained in the [docs](https://developer.twitter.com/en/docs/tweets/optimize-with-cards/overview/abouts-cards), it's enough to add to the `head` of the HTML document the following stuff:

```html
<meta name="twitter:card" content="summary" />
<meta name="twitter:site" content="@nytimesbits" />
<meta name="twitter:creator" content="@nickbilton" />
<meta property="og:url" content="http://bits.blogs.nytimes.com/2011/12/08/a-twitter-for-my-sister/" />
<meta property="og:title" content="A Twitter for My Sister" />
<meta property="og:description" content="In the early days, Twitter grew so quickly that it was almost impossible to add new features because engineers spent their time trying to keep the rocket ship from stalling." />
<meta property="og:image" content="http://graphics8.nytimes.com/images/2011/12/08/technology/bits-newtwitter/bits-newtwitter-tmagArticle.jpg" />
```

In Hakyll we can add the meta tags to `templates/default.html`. We just need to make sure to have them only in the post pages. To make that happen, we just check if the `description` key is present in the context:

```html
$if(description)$
  <meta name="twitter:card" content="summary" />
  <meta name="twitter:site" content="MY_TWITTER_HANDLE" />
  <meta name="twitter:creator" content="MY_TWITTER_HANDLE" />
  <meta property="og:url" content="$url$" />
  <meta property="og:title" content="$title$" />
  <meta property="og:description" content="$description$" />
  <meta property="og:image" content="$cover_image$" />
$else$
$endif$
```

And then in each post metadata we need to add `description` and `cover_image`:

```markdown
---
...
description: Some description of my post
cover_image: https://example.com/cover_image
---
```
