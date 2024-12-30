---
title: Merging IO and Either into one monad
description: How to simplify code by merging two monads into one
author: Riccardo
cover_image: /images/puzzle-merge.jpg
tags:
  - Functional Programming
  - Haskell
  - Monad
---

A good way to metabolize new concepts is to copy ideas from other people and try to make them compile. This is exactly what we are going to do today.

In case you wanted to read the (better) original, please check out ["A Gentle Introduction to Monad Transformers"](https://two-wrongs.com/a-gentle-introduction-to-monad-transformers).

## The Problem

Let's write a function to extract the domain from an email:

```hs
getDomain :: Text -> Either LoginError Text
getDomain email =
  case T.splitOn "@" email of
    [_, domain] -> Right domain
    _           -> Left InvalidEmail

```

Then let's say we want to ask the user for their email and use the domain as a token for authentication:

```hs
getToken :: IO (Either LoginError Text)
getToken = do
  T.putStrLn "Enter email address:"
  email <- T.getLine
  pure $ getDomain email
```

Besides the token, the user need to input a password. The database of users is the following:

```hs
users :: Map Text Text
users = Map.fromList
  [ ("example.com", "qwerty123")
  , ("localhost", "password")
  ]
```

For our authentication system three possible errors are possible:

```hs
data LoginError
  = InvalidEmail
  | NoSuchUser
  | WrongPassword
  deriving (Show)
```

Finally, we can put all together:

```hs
userLogin :: IO (Either LoginError Text)
userLogin = do
  token <- getToken
  case token of
    Right domain ->
      case Map.lookup domain users of
        Just password -> do
          T.putStrLn "Enter password:"
          pw <- T.getLine
          if pw == password then
            pure $ Right domain
          else
            pure $ Left WrongPassword
        Nothing ->
          pure $ Left NoSuchUser
    left    ->
      pure $ left
```

And we are done!

Nope, it's a joke. That piece of code really is ugly. We need to do something about it.

There's one secret to clean up `userLogin`: the indentation comes from the fact that we are dealing with two different monads (i.e. `IO` and `Either`).

Let's see if merging the two into one solves our issue.

## The Solution

We first wrap the two monads into `EitherIO`:

```hs
data EitherIO e a = EitherIO {
  runEitherIO :: IO (Either e a)
}
```

Then we define instances for `Functor`, `Applicative` and `Monad`:

```hs
instance Functor (EitherIO e) where
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  -- fmap g f = EitherIO $ fmap (fmap g) (runEitherIO f)
  fmap g = EitherIO . fmap (fmap g) . runEitherIO

-- THIS IS NOT LAWFUL. DO NOT USE FOR SERIOUS STUFF!
instance Applicative (EitherIO e) where
  -- pure :: a -> f a
  pure = EitherIO . pure . Right
  -- (<*>) :: f (a -> b) -> f a -> f b
  fg <*> f = EitherIO $ liftA2 (<*>) (runEitherIO fg) (runEitherIO f)

instance Monad (EitherIO e) where
  -- (>>=) :: forall a b. m a -> (a -> m b) -> m b
  m >>= mg = EitherIO $ (runEitherIO m >>= either (pure . Left) (runEitherIO . mg))
```

We can define `getToken` in terms of our new monad:

```hs
getToken' :: EitherIO LoginError Text
getToken' = do
  EitherIO $ fmap Right (T.putStrLn "Enter email address:")
  email <- EitherIO $ fmap Right T.getLine
  EitherIO $ pure $ getDomain email
```

Even better, we can create a couple of helpers to make it cleaner

```hs
liftEither :: Either e a -> EitherIO e a
liftEither = EitherIO . pure

liftIO :: IO a -> EitherIO e a
liftIO = EitherIO . fmap Right

getToken'' :: EitherIO LoginError Text
getToken'' = do
  liftIO $ T.putStrLn "Enter email address:"
  email <- liftIO T.getLine
  liftEither $ getDomain email
```

Now, `userLogin` can be rewritten as:

```hs
userLogin' :: EitherIO LoginError Text
userLogin' = do
  domain <- getToken''
  case Map.lookup domain users of
    Just password -> do
      liftIO $ T.putStrLn "Enter password:"
      pw <- liftIO T.getLine
      if pw == password then
        liftEither $ Right domain
      else
        liftEither $ Left WrongPassword
    Nothing ->
      liftEither $ Left NoSuchUser
```

We have removed one level of nesting. That is because with the Monad instance of `EitherIO`, we "extract" the domain with `<- getToken''` and not the `Either _ domain` as before. But we can do even better:

```hs
userLogin'' :: EitherIO LoginError Text
userLogin'' = do
  domain <- getToken''
  password <- maybe (liftEither $ Left NoSuchUser) pure $ Map.lookup domain users
  liftIO $ T.putStrLn "Enter password:"
  pw <- liftIO T.getLine
  if pw == password then
    liftEither $ Right domain
  else
    liftEither $ Left WrongPassword
```

With all of that in place we can run the login with:

```hs
main :: IO ()
main = do
  result <- runEitherIO getToken''
  print result
```

## But wait, there's more!

We could refactor `userLogin` to:

```hs
throwE :: e -> EitherIO e a
throwE = liftEither . Left

userLogin''' :: EitherIO LoginError Text
userLogin''' = do
  domain <- getToken''
  password <- maybe (throwE NoSuchUser) pure $ Map.lookup domain users
  liftIO $ T.putStrLn "Enter password:"
  pw <- liftIO T.getLine
  if pw == password then
    liftEither $ Right domain
  else
    throwE WrongPassword
```

But if we can throw, we can also catch:

```hs
catchE :: EitherIO e a -> (e -> EitherIO e a) -> EitherIO e a
catchE throwing handler = EitherIO $ do
  result <- runEitherIO throwing
  case result of
    Left  e -> runEitherIO $ handler e
    success -> pure success
```

And have a handler that allows the user to retry the login in case of `WrongPassword` error:

```hs
wrongPasswordHandler :: LoginError -> EitherIO LoginError Text
wrongPasswordHandler WrongPassword = do
  liftIO $ T.putStrLn "Wrong password, one more chance."
  userLogin'''
wrongPasswordHandler e = throwE e
```

With that we can:

```hs
main :: IO ()
main = do
  result <- runEitherIO $ userLogin''' `catchE` wrongPasswordHandler
  print result

```

## The whole code

```hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text as T
import Data.Text.IO as T
import Data.Map as Map
import Data.Either (either)
import Data.Maybe (maybe)
import Control.Applicative (liftA2)

data LoginError
  = InvalidEmail
  | NoSuchUser
  | WrongPassword
  deriving (Show)

getDomain :: Text -> Either LoginError Text
getDomain email =
  case T.splitOn "@" email of
    [_, domain] -> Right domain
    _           -> Left InvalidEmail

getToken :: IO (Either LoginError Text)
getToken = do
  T.putStrLn "Enter email address:"
  email <- T.getLine
  pure $ getDomain email

users :: Map Text Text
users = Map.fromList
  [ ("example.com", "qwerty123")
  , ("localhost", "password")
  ]

userLogin :: IO (Either LoginError Text)
userLogin = do
  token <- getToken
  case token of
    Right domain ->
      case Map.lookup domain users of
        Just password -> do
          T.putStrLn "Enter password:"
          pw <- T.getLine
          if pw == password then
            pure $ Right domain
          else
            pure $ Left WrongPassword
        Nothing ->
          pure $ Left NoSuchUser
    left    ->
      pure $ left

data EitherIO e a = EitherIO {
  runEitherIO :: IO (Either e a)
}

instance Functor (EitherIO e) where
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  -- fmap g f = EitherIO $ fmap (fmap g) (runEitherIO f)
  fmap g = EitherIO . fmap (fmap g) . runEitherIO

-- THIS IS NOT LAWFUL. DO NOT USE FOR SERIOUS STUFF!
instance Applicative (EitherIO e) where
  -- pure :: a -> f a
  pure = EitherIO . pure . Right
  -- (<*>) :: f (a -> b) -> f a -> f b
  fg <*> f = EitherIO $ liftA2 (<*>) (runEitherIO fg) (runEitherIO f)

instance Monad (EitherIO e) where
  -- (>>=) :: forall a b. m a -> (a -> m b) -> m b
  m >>= mg = EitherIO $ (runEitherIO m >>= either (pure . Left) (runEitherIO . mg))

getToken' :: EitherIO LoginError Text
getToken' = do
  EitherIO $ fmap Right (T.putStrLn "Enter email address:")
  email <- EitherIO $ fmap Right T.getLine
  EitherIO $ pure $ getDomain email

liftEither :: Either e a -> EitherIO e a
liftEither = EitherIO . pure

liftIO :: IO a -> EitherIO e a
liftIO = EitherIO . fmap Right

getToken'' :: EitherIO LoginError Text
getToken'' = do
  liftIO $ T.putStrLn "Enter email address:"
  email <- liftIO T.getLine
  liftEither $ getDomain email

userLogin' :: EitherIO LoginError Text
userLogin' = do
  domain <- getToken''
  case Map.lookup domain users of
    Just password -> do
      liftIO $ T.putStrLn "Enter password:"
      pw <- liftIO T.getLine
      if pw == password then
        liftEither $ Right domain
      else
        liftEither $ Left WrongPassword
    Nothing ->
      liftEither $ Left NoSuchUser

userLogin'' :: EitherIO LoginError Text
userLogin'' = do
  domain <- getToken''
  password <- maybe (liftEither $ Left NoSuchUser) pure $ Map.lookup domain users
  liftIO $ T.putStrLn "Enter password:"
  pw <- liftIO T.getLine
  if pw == password then
    liftEither $ Right domain
  else
    liftEither $ Left WrongPassword

throwE :: e -> EitherIO e a
throwE = liftEither . Left

userLogin''' :: EitherIO LoginError Text
userLogin''' = do
  domain <- getToken''
  password <- maybe (throwE NoSuchUser) pure $ Map.lookup domain users
  liftIO $ T.putStrLn "Enter password:"
  pw <- liftIO T.getLine
  if pw == password then
    liftEither $ Right domain
  else
    throwE WrongPassword

catchE :: EitherIO e a -> (e -> EitherIO e a) -> EitherIO e a
catchE throwing handler = EitherIO $ do
  result <- runEitherIO throwing
  case result of
    Left  e -> runEitherIO $ handler e
    success -> pure success

wrongPasswordHandler :: LoginError -> EitherIO LoginError Text
wrongPasswordHandler WrongPassword = do
  liftIO $ T.putStrLn "Wrong password, one more chance."
  userLogin'''
wrongPasswordHandler e = throwE e

main :: IO ()
main = do
  print $ getDomain "a"
  print $ getDomain "a@b"
  t <- getToken
  print t
  result <- userLogin
  print result
  t' <- runEitherIO getToken''
  print t'
  t'' <- runEitherIO $ userLogin''' `catchE` wrongPasswordHandler
  print t''
```
