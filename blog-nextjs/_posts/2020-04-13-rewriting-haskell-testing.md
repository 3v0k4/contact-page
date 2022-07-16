---
title: Rewriting to Haskell–Testing
description: Using HSpec to test Stream through its endpoints
author: Riccardo
series: Rewriting to Haskell
tags:
  - Functional Programming
  - Haskell
  - Servant
---

We have managed to delay testing by [leaning on Ruby RSpec](https://odone.io/posts/2020-03-30-rails.html) for a while. It's time to do the right thing and write some tests in Haskell.

We peaked at the ["How To Test Servant Applications"](https://docs.servant.dev/en/stable/cookbook/testing/Testing.html) cookbook page and decided to go the [`hspec-wai`](https://hackage.haskell.org/package/hspec-wai) way. In fact, using `servant-client` seemed a bit too involved and coupled with Servant. The former, on the other hand, enables us to test any wai application.

With that in mind we created a `test/Spec.hs`:

```hs
main :: IO ()
main = do
  setEnv "DATABASE" "stream_test"
-- ^ See https://odone.io/posts/2020-03-23-rewriting-haskell-configuration.html for the why.
  application <- configuredApp
  connection <- getConnection
  hspec $ spec application connection

spec :: Application -> Connection -> Spec
spec application connection = with (pure application) $ after_ (truncateTables connection)
--                                                      ^ After each spec item truncate tables.
  $ describe "GET /servant/search"
  $ do
    it "with no query it returns all posts ordered by descending creation date" $ do
      user <- liftIO . createUser $ connection
      now <- liftIO randomUTCTime
      let later = addUTCTime 1 now
      let olderPostAttributes =
            defaultPostAttributes
              { postAttributesUserId = userId user,
                postAttributesCreatedAt = Just now
              }
      let newerPostAttributes = olderPostAttributes {postAttributesCreatedAt = Just later}
      olderPost <- liftIO $ createPost connection olderPostAttributes
      newerPost <- liftIO $ createPost connection newerPostAttributes
      get "/servant/search"
        `shouldRespondWith` [json|
--                          ^ Using hspec-wai-json and some Template Haskell to generate JSON.
        { users: [#{user}],
--                ^ Behind the curtains aeson-qq is used. Thus, you can interpolate variables:
--                  https://github.com/sol/aeson-qq#aeson-qq-json-quasiquoter-for-haskell
          attachments: [],
          comments: [],
          posts: [#{newerPost}, #{olderPost}]
        }
      |]

-- ...
```

At the moment we are just testing through the endpoints as shown above. Stream is a simple application so we feel confident with this approach. Should the need for other types of tests arise, then [HSpec](https://hspec.github.io/) will have our backs.
