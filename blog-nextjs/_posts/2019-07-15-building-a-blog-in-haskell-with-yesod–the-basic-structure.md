---
title: Building a Blog in Haskell with Yesod–The Basic Structure
author: Riccardo
description: In this post we are going to develop the basic structure of our Yesod blog
cover_image: /images/yesod.png
series: Building a Blog in Haskell with Yesod
tags:
  - Functional Programming
  - Haskell
  - Yesod
---

This is a series about [Yesod](https://www.yesodweb.com/): a Haskell web framework that follows a similar philosophy to [Rails](https://rubyonrails.org/). In fact, it is strongly opinionated and provides a lot of functionality out of the box.

A good read about Yesod is available online for free: [Developing web applications with Haskell and Yesod](https://www.yesodweb.com/book). That's why this series will be a commentary of the commits from a [repo](https://github.com/3v0k4/yesod-blog) we will use to develop a super simple blog.

In other words, this won't be good material to learn how to use Yesod. However, it will hopefully give an overview of how the framework works.

---

## Intro

In this post we are going to develop the basic structure of our blog. The flow of the user is simple:

1. The landing page is a login form:

![Screenshot of a login form with username and password fields and a submit button](/images/login.png)

2. After a successful login the user is redirected to the posts page. Here, the user is able to create a new post and check all the posts already published:

![Screenshot of the blog with a "toggle form" button and two posts with title and text](/images/posts-final.png)

To keep this article simple, we are not going to connect to the database, neither we will check the login credentials.

## Init

Commit [7a5782d47e082a8cfcb4e07bac71d88709179c0c](https://github.com/3v0k4/yesod-blog/commit/7a5782d47e082a8cfcb4e07bac71d88709179c0c) just implements the steps listed in the [Yesod quick start guide](https://www.yesodweb.com/page/quickstart).

## The Landing Page

The `stack exec -- yesod add-handler` command allows to create a new route and handler. In other words, the logic that takes care of a request at a specific URL.

Commit [354fac670be4b9869b171a0fdd7d15063d094fdb](https://github.com/3v0k4/yesod-blog/commit/354fac670be4b9869b171a0fdd7d15063d094fdb) uses that command to add the landing page route

```diff
diff --git a/config/routes b/config/routes
index 37e8bea..df5e055 100644
--- a/config/routes
+++ b/config/routes
@@ -12,3 +12,4 @@
 /comments CommentR POST
 
 /profile ProfileR GET
+/landing LandingR GET
```

and handler:

```diff
diff --git a/src/Handler/Landing.hs b/src/Handler/Landing.hs
new file mode 100644
index 0000000..8a1fbdc
--- /dev/null
+++ b/src/Handler/Landing.hs
@@ -0,0 +1,6 @@
+module Handler.Landing where
+
+import Import
+
+getLandingR :: Handler Html
+getLandingR = error "Not yet implemented: getLandingR"
```

## Authorization

Yesod provides authentication / authorization support out of the box. That is why the application doesn't compile:

```bash
    Pattern match(es) are non-exhaustive
    In an equation for ‘isAuthorized’: Patterns not matched: LandingR _
    |
163 |     isAuthorized (AuthR _) _ = return Authorized
    |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^...
```

In particular, we are missing `isAuthorized` for the new `LandingR` handler. Let's fix it by always allowing access:

```diff
diff --git a/src/Foundation.hs b/src/Foundation.hs
index f91122a..011e171 100644
--- a/src/Foundation.hs
+++ b/src/Foundation.hs
@@ -171,6 +171,8 @@ instance Yesod App where
     -- delegate to that function
     isAuthorized ProfileR _ = isAuthenticated

+    isAuthorized LandingR _ = return Authorized
+
     -- This function creates static content files in the static folder
     -- and names them based on a hash of their content. This allows
     -- expiration dates to be set far in the future without worry of
```

With that, it's possible to launch the dev server `stack exec -- yesod devel` and visit `/landing`.

![Screenshot of a compilation error in the browser that says "Not yet implemented: getLandingR"](/images/landing-error.png)

The error comes from the fact that the handler is not yet developed.

## Landing Somewhere

In the spirit of doing baby steps, let's first put something on screen for `/landing`. Commit [5271b5f514782f659c89101ee0c1cd4f8d691ade](https://github.com/3v0k4/yesod-blog/commit/5271b5f514782f659c89101ee0c1cd4f8d691ade) introduces an Hamlet template which is used to generate HTML:

```diff
diff --git a/templates/landing.hamlet b/templates/landing.hamlet
new file mode 100644
index 0000000..af3d162
--- /dev/null
+++ b/templates/landing.hamlet
@@ -0,0 +1,2 @@
+<h1>
+  Landing #{interpolated}
```

In particular, `#{interpolated}` comes from the handler:

```diff
diff --git a/src/Handler/Landing.hs b/src/Handler/Landing.hs
index 8a1fbdc..aefaa65 100644
--- a/src/Handler/Landing.hs
+++ b/src/Handler/Landing.hs
@@ -1,6 +1,15 @@
+{-# LANGUAGE NoImplicitPrelude #-}
+{-# LANGUAGE OverloadedStrings #-}
+{-# LANGUAGE TemplateHaskell #-}
+{-# LANGUAGE MultiParamTypeClasses #-}
+{-# LANGUAGE TypeFamilies #-}
+
 module Handler.Landing where
 
 import Import
 
 getLandingR :: Handler Html
-getLandingR = error "Not yet implemented: getLandingR"
+getLandingR = do
+  defaultLayout $ do
+    let interpolated = "interpolated string" :: Text
+    $(widgetFile "landing")
```

In other words, every variable in scope can be used in the template.

If we visited `/landing` now we would see: ![Screenshot of the landing page showing a "Landing interpolated string" heading](/images/landing-interpolation.png)

## Tweaking the Landing

Commit [bb89035c9e4f4b9265bebe378ec38e932bc47bcd](https://github.com/3v0k4/yesod-blog/commit/bb89035c9e4f4b9265bebe378ec38e932bc47bcd) does a couple of things:

1. Puts the `LandingR` handler at `/`:

```diff
diff --git a/config/routes b/config/routes
index df5e055..c2d4fd2 100644
--- a/config/routes
+++ b/config/routes
@@ -7,9 +7,9 @@
 /favicon.ico FaviconR GET
 /robots.txt RobotsR GET
 
-/ HomeR GET POST
+/home HomeR GET POST
 
 /comments CommentR POST
 
 /profile ProfileR GET
-/landing LandingR GET
+/ LandingR GET
```

2. Creates an `emptyLayout`. This is done to remove all the noise created while initializing the Yesod application so that the code is easier to navigate. We won't get into details, the important thing is that we can now use `emptyLayout` instead of `defaultLayout`.

```diff
diff --git a/src/Handler/Landing.hs b/src/Handler/Landing.hs
index aefaa65..7675a0f 100644
--- a/src/Handler/Landing.hs
+++ b/src/Handler/Landing.hs
@@ -7,9 +7,15 @@
 module Handler.Landing where
 
 import Import
+import Text.Hamlet          (hamletFile)
+
+emptyLayout :: Widget -> Handler Html
+emptyLayout widget = do
+    pc <- widgetToPageContent $ do
+        $(widgetFile "empty-layout")
+    withUrlRenderer $(hamletFile "templates/empty-layout-wrapper.hamlet")
 
 getLandingR :: Handler Html
 getLandingR = do
-  defaultLayout $ do
-    let interpolated = "interpolated string" :: Text
+  emptyLayout $ do
     $(widgetFile "landing")
diff --git a/templates/empty-layout-wrapper.hamlet b/templates/empty-layout-wrapper.hamlet
new file mode 100644
index 0000000..18bb787
--- /dev/null
+++ b/templates/empty-layout-wrapper.hamlet
@@ -0,0 +1,5 @@
+\<!doctype html>
+<html>
+  <head>
+  <body>
+    ^{pageBody pc}
diff --git a/templates/empty-layout.hamlet b/templates/empty-layout.hamlet
new file mode 100644
index 0000000..42cfe82
--- /dev/null
+++ b/templates/empty-layout.hamlet
@@ -0,0 +1,2 @@
+<div .container>
+  ^{widget}
```

Also, we get ready to add the login form:

```diff
diff --git a/templates/landing.hamlet b/templates/landing.hamlet
index af3d162..24a77b3 100644
--- a/templates/landing.hamlet
+++ b/templates/landing.hamlet
@@ -1,2 +1 @@
-<h1>
-  Landing #{interpolated}
+<h1>Login
```

If we visited `/` now we would see just a "Login" heading.

## The Login Form

Commit [8d89a7101a979b06ea324c8d6d9aa0373f49d6ee](https://github.com/3v0k4/yesod-blog/commit/8d89a7101a979b06ea324c8d6d9aa0373f49d6ee) adds the login form logic.

In particular, we first add the posts route and handler with `stack exec -- yesod add-handler`. This will be the page to redirect too after a successful login. Also, we add a POST handler to `LandingR` to handle the form submit:

```diff
diff --git a/config/routes b/config/routes
index c2d4fd2..db4ac58 100644
--- a/config/routes
+++ b/config/routes
@@ -12,4 +12,5 @@
 /comments CommentR POST
 
 /profile ProfileR GET
-/ LandingR GET
+/ LandingR GET POST
+/posts PostsR GET
```

Then we allow anybody to access `PostR`. We will limit this resource to logged in users in the future.

```diff
diff --git a/src/Application.hs b/src/Application.hs
index 99dbb26..6045a41 100644
--- a/src/Application.hs
+++ b/src/Application.hs
@@ -45,6 +45,7 @@ import Handler.Home
 import Handler.Comment
 import Handler.Profile
 import Handler.Landing
+import Handler.Posts
 
 -- This line actually creates our YesodDispatch instance. It is the second half
 -- of the call to mkYesodData which occurs in Foundation.hs. Please see the
diff --git a/src/Foundation.hs b/src/Foundation.hs
index 011e171..e96228a 100644
--- a/src/Foundation.hs
+++ b/src/Foundation.hs
@@ -173,6 +173,8 @@ instance Yesod App where
 
     isAuthorized LandingR _ = return Authorized
 
+    isAuthorized PostsR _ = return Authorized
+
```

We add the login form. Important pieces here:

- The type-safe url `@{LandingR}`: the form gets submitted to the POST `LandingR` handler
- The interpolated form `^{widget}` that comes from the GET `LandingR` handler

```diff
diff --git a/templates/landing.hamlet b/templates/landing.hamlet
index 24a77b3..8df39e9 100644
--- a/templates/landing.hamlet
+++ b/templates/landing.hamlet
@@ -1 +1,4 @@
 <h1>Login
+<form method=post action=@{LandingR} enctype=#{enctype}>
+  ^{widget}
+  <button>Login
```

The form gets generated in the GET `LandingR` handler using `generateFormPost`. In other words, a form that will be submitted via POST.

Subsequently, POST `LandingR` handler takes the submitted form parameters and runs the validation with `runFormPost`. In case of success, the user is redirected to the posts page. Otherwise, the login page is re-rendered with the validation errors.

```diff
diff --git a/src/Handler/Landing.hs b/src/Handler/Landing.hs
index 7675a0f..c2d4d10 100644
--- a/src/Handler/Landing.hs
+++ b/src/Handler/Landing.hs
@@ -3,19 +3,33 @@
 {-# LANGUAGE TemplateHaskell #-}
 {-# LANGUAGE MultiParamTypeClasses #-}
 {-# LANGUAGE TypeFamilies #-}
+{-# LANGUAGE OverloadedStrings #-}
 
 module Handler.Landing where
 
 import Import

+data Login =
+  Login { username :: Text, password :: Text }
+  deriving Show
+
+loginForm :: Form Login
+loginForm =
+  renderDivs $
+    Login <$> areq textField "Username" Nothing <*> areq textField "Password" Nothing
 
 getLandingR :: Handler Html
 getLandingR = do
+  (widget, enctype) <- generateFormPost loginForm
   emptyLayout $ do
     $(widgetFile "landing")
+
+postLandingR :: Handler Html
+postLandingR = do
+  ((result, widget), enctype) <- runFormPost loginForm
+  case result of
+    FormSuccess _ ->
+      redirect PostsR
+    _ ->
+      emptyLayout $ do
+        $(widgetFile "landing")
```

If we visited `/` now we would see: ![Screenshot of a login form with username and password fields and a submit button](/images/login.png)

## The Posts Page

Since the posts page was created with `stack exec -- yesod add-handler`, it only has an empty handler:

```diff
diff --git a/src/Handler/Posts.hs b/src/Handler/Posts.hs
new file mode 100644
index 0000000..138f43b
--- /dev/null
+++ b/src/Handler/Posts.hs
@@ -0,0 +1,6 @@
+module Handler.Posts where
+
+import Import
+
+getPostsR :: Handler Html
+getPostsR = error "Not yet implemented: getPostsR"
```

Commit [a176ea0d10b63b9613cbfc5bbf86fbe264461111](https://github.com/3v0k4/yesod-blog/commit/a176ea0d10b63b9613cbfc5bbf86fbe264461111) fixes it by tweaking the posts page. In particular, it adds a couple of hardcoded posts and a form to create a new one.

The structure is exactly the same as seen above for the login page:

```diff
diff --git a/config/routes b/config/routes
index db4ac58..5a01ba2 100644
--- a/config/routes
+++ b/config/routes
@@ -13,4 +13,4 @@
 
 /profile ProfileR GET
 / LandingR GET POST
-/posts PostsR GET
+/posts PostsR GET POST
diff --git a/src/Handler/Posts.hs b/src/Handler/Posts.hs
index 138f43b..8d0b8c4 100644
--- a/src/Handler/Posts.hs
+++ b/src/Handler/Posts.hs
@@ -1,6 +1,35 @@
+{-# LANGUAGE NoImplicitPrelude #-}
+{-# LANGUAGE OverloadedStrings #-}
+{-# LANGUAGE TemplateHaskell #-}
+{-# LANGUAGE MultiParamTypeClasses #-}
+{-# LANGUAGE TypeFamilies #-}
+{-# LANGUAGE OverloadedStrings #-}
+
 module Handler.Posts where
 
 import Import
 
+data Post =
+  Post { title :: Text, text :: Text }
+  deriving Show
+
+postForm :: Form Post
+postForm =
+  renderDivs $
+    Post <$> areq textField "Title" Nothing <*> areq textField "Text" Nothing
+
 getPostsR :: Handler Html
-getPostsR = error "Not yet implemented: getPostsR"
+getPostsR = do
+  (widget, enctype) <- generateFormPost postForm
+  emptyLayout $ do
+    $(widgetFile "posts")
+
+postPostsR :: Handler Html
+postPostsR = do
+  ((result, widget), enctype) <- runFormPost postForm
+  case result of
+    FormSuccess _ ->
+      redirect PostsR
+    _ ->
+      emptyLayout $ do
+        $(widgetFile "posts")
diff --git a/templates/posts.hamlet b/templates/posts.hamlet
new file mode 100644
index 0000000..2445442
--- /dev/null
+++ b/templates/posts.hamlet
@@ -0,0 +1,15 @@
+<h1>Posts
+
+<div id="new-post">
+  <form method=post action=@{PostsR} enctype=#{enctype}>
+    ^{widget}
+    <button>Post
+
+<ul>
+  <li>
+    <h2>Title 1
+    <p>Text 1
+
+  <li>
+    <h2>Title 2
+    <p>Text 2
```

If we visited `/posts` now we would see: ![Screenshot of the blog with a form at the top with title field, text field and submit button; below two hardcoded posts with title and text](/images/posts.png)

## Tweaking the Empty Layout

Commit [77567ed74b12685bbf319a6764f1ff83ac9604b8](https://github.com/3v0k4/yesod-blog/commit/77567ed74b12685bbf319a6764f1ff83ac9604b8) adds an important element to the empty layout. We won't get into details but it is going to be important later when adding CSS.

```diff
diff --git a/templates/empty-layout-wrapper.hamlet b/templates/empty-layout-wrapper.hamlet
index 18bb787..c98c0a0 100644
--- a/templates/empty-layout-wrapper.hamlet
+++ b/templates/empty-layout-wrapper.hamlet
@@ -1,5 +1,6 @@
 \<!doctype html>
 <html>
   <head>
+    ^{pageHead pc}
   <body>
     ^{pageBody pc}
```

## Adding JavaScript and CSS to Posts

Commit [5fb4b964e0890a042b868738d32a99680c06836e](https://github.com/3v0k4/yesod-blog/commit/5fb4b964e0890a042b868738d32a99680c06836e) shows how to add JavaScript and CSS to a template.

Up until now, we've been using `$(widgetFile "NAME")` to render Hamlet templates. Turns out the `widgetFile` looks for other types of templates and puts them together.  In fact, by using Julius and Cassius templates we can add respectively JavaScript and CSS.

The following change introduces a button that allows to show / hide the post form with a button:

```diff
diff --git a/templates/posts.cassius b/templates/posts.cassius
new file mode 100644
index 0000000..f16f327
--- /dev/null
+++ b/templates/posts.cassius
@@ -0,0 +1,2 @@
+#new-post
+  display: none
diff --git a/templates/posts.hamlet b/templates/posts.hamlet
index 2445442..82e717c 100644
--- a/templates/posts.hamlet
+++ b/templates/posts.hamlet
@@ -1,5 +1,8 @@
 <h1>Posts
 
+<button id="toggle-new-post">
+  Toggle Form
+
 <div id="new-post">
   <form method=post action=@{PostsR} enctype=#{enctype}>
     ^{widget}
diff --git a/templates/posts.julius b/templates/posts.julius
new file mode 100644
index 0000000..3790b61
--- /dev/null
+++ b/templates/posts.julius
@@ -0,0 +1,10 @@
+document.addEventListener("DOMContentLoaded", function() {
+  document.getElementById("toggle-new-post").addEventListener("click", function() {
+    var value =
+      document.getElementById("new-post").style.display === "block"
+      ? "none"
+      : "block";
+
+    document.getElementById("new-post").style.display = value;
+  });
+});
```

If we visited `/` now we would see: ![Screenshot of the blog with a "toggle form" button and two posts with title and text](/images/posts-final.png)
