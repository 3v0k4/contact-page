---
title: Scripting the Hell out of Trello with Haskell
description: Want to use Haskell for something real but don't want to risk it at work? This is how I do it.
published: true
author: Riccardo
cover_image: https://odone.io/images/bash.jpg
tags:
  - Functional Programming
  - Script
  - Haskell
---

When I started learning Haskell, I struggled to find places to deploy it. Doing silly textbook exercises was boring. At the same time, committing substantial work to it would have been a risk.

Somebody encouraged me to try writing scripts at Monadic Party last year. I [never looked back](https://odone.io/tags/Script.html): it's approachable, useful, and fun.

Let's write one step-by-step. If you cannot resist the urge, feel free to [go to the bottom](#the-whole-script) to see the final result.

## The Problem

Trello is excellent when it comes to one-off cards. Unfortunately, it doesn't offer any support for recurring tasks out of the box.

Sure, there's probably more than one power-up for that, but I'm not passing on the chance of scripting the shit out of my productivity system with Haskell.

We will develop a script that:

1. Fetches templates and their intervals from a list of "recurring cards".
2. Creates cards for the upcoming week in a target list.

For example, I send the [PinkLetter](https://odone.io/#newsletter) on Sundays, check my Twitter stats on the first of the month, and YouTube channel on the fifteenth. In Trello, I keep three template cards named "Sun | Send PinkLetter", "D01 | Check Twitter stats", and "D15 | Check YouTube channel".

The script is aware of what day is today, so it only creates tasks that are due in the next seven days. Also, all the labels included in the templates are preserved.

To hell with explanations; let's fire up the editor! VIM, right?

## The Script, Step-By-Step

### Hello, World!

The first thing to do when creating a new piece of code is to appease the programming Gods. Yeah, it sucks, but it's either a Ph.D. in math or this to write Haskell.

We create a file and make it executable:

```bash
touch recur.hs
chmod +x recur.hs
```

Then, we install [Stack](https://docs.haskellstack.org/en/stable/README/) and shamelessly copy from the [README](https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter):

```hs
#!/usr/bin/env stack
{- stack
  script
  --resolver lts-16.25
-}

main :: IO ()
main = putStrLn "Hello, World!"
```

Notice we are using the latest [resolver](https://www.stackage.org). Actually, as of today, 16.26 is out. Damn, those people at FP Complete are always at work!

```bash
./recur.hs
# Hello, World!
```

### HTTP GET

Imagine doing a type-safe HTTP GET request in Bash. Now, let's get serious and do some Haskell.

Picasso once said something like "Good programmers copy, great programmers steal." Who am I to judge that? From [req's docs](https://hackage.haskell.org/package/req):

```hs
#!/usr/bin/env stack
{- stack
  script
  --resolver lts-16.25
  --package req
  --package aeson
  --package text
-}

{-# LANGUAGE OverloadedStrings #-}

main :: IO ()
main = httpGet

httpGet :: IO ()
httpGet =
  runReq defaultHttpConfig $ do
    r <-
      req
        GET
        (https "httpbin.org" /: "get")
        NoReqBody
        jsonResponse
        mempty
    liftIO $ print (responseBody r :: Value)
```

The `httpbin.org/get` endpoint echoes back the request to us:

```bash
./recur.hs
# Object (fromList [("args",Object (fromList [])),("url",String "https://httpbin.org/get"),...]))])
```

### GET a Card

We need to fetch all the templates, but this is not a draw-an-owl tutorial. The first step is to retrieve one card.

Trello provides the `/1/cards/:cardId` endpoint for that. Should you wish to get a `cardId` from a board of yours, just open it in the browser and add `.json` at the end of the URL.

```hs
key :: Text
key = "..."

token :: Text
token = "..."

cardId :: Text
cardId = "..."

httpGet :: IO ()
httpGet =
  runReq defaultHttpConfig $ do
    r <-
      req
        GET
        (https "trello.com" /: "1" /: "cards" /: cardId) -- CHANGED
        NoReqBody
        jsonResponse
        ("key" =: key <> "token" =: token) -- CHANGED
    liftIO $ print (responseBody r :: Value)
```

Unfortunately, executing the script returns a bunch of garbage:

```bash
./recur.hs
# Object (fromList [("email",Null),("subscribed",Bool False),("due",String "2020-12-20T09:00:00.000Z"),("checkItemStates",Array []),("closed",Bool True),("idMembersVoted",Array []),("isTemplate",Bool False),("cover",Object(fromList [("color",Null),("size",String "normal"),("idAttachment",Null),("brightness",String "light"),("idUploadedBackground",Null)])),("idLabels",Array [String "5fd7218e1258da48af1ceb26"]),("start",Null),("url",String"https://trello.com/c/FX6SrQUD/7-send-pinkletter"),("badges",Object (fromList [("subscribed",Bool False),("due",String "2020-12-20T09:00:00.000Z"),("checkItemsChecked",Number 0.0),("attachments",Number 0.0),("checkItemsEarliestDue",Null),("location",Bool False),("checkItems",Number 0.0),("start",Null),("votes",Number 0.0),("viewingMemberVoted",Bool False),("fogbugz",String ""),("dueComplete",Bool False),("comments",Number 0.0),("description",Bool False),("attachmentsByType",Object (fromList [("trello",Object (fromList [("card",Number 0.0),("board",Number 0.0)]))]))])),("pos",Number 49152.0),("name",String "Sun | Send PinkLetter"),("idList",String "5fd721a3cc7a515c3e0af1fb"),("idShort",Number 7.0),("idBoard",String "5fd7218ec8cfa310d9fc2571"),("shortLink",String "FX6SrQUD"),("idChecklists",Array []),("dueReminder",Null),("id",String "5fd726a896443d2aa3c027dd"),("labels",Array [Object (fromList [("color",String "yellow"),("name",String "Craft"),("idBoard",String "5fd7218ec8cfa310d9fc2571"),("id",String "5fd7218e1258da48af1ceb26")])]),("dueComplete",Bool False),("cardRole",Null),("dateLastActivity",String "2020-12-14T08:47:43.963Z"),("idMembers",Array []),("manualCoverAttachment",Bool False),("descData",Null),("desc",String ""),("shortUrl",String "https://trello.com/c/FX6SrQUD"),("idAttachmentCover",Null)])
```

### Parse Before It's Too Late

We don't want to welcome unstructured data into our beautiful core logic. But what to do? That's right, we right-click some inspiration from [aeson](https://hackage.haskell.org/package/aeson-1.5.4.1/docs/Data-Aeson.html):

```hs
{-# LANGUAGE DeriveGeneric #-}

data Card = Card
  { name :: Text
  }
  deriving (Generic, Show)

instance FromJSON Card
```

Since [`HttpResponse`](https://hackage.haskell.org/package/req-3.8.0/docs/Network-HTTP-Req.html#t:HttpResponse) has an instance for

```hs
FromJSON a => HttpResponse (JsonResponse a)
```

we just need to change one type:

```hs
httpGet :: IO ()
httpGet =
  runReq defaultHttpConfig $ do
    r <-
      req
        GET
        (https "trello.com" /: "1" /: "cards" /: cardId)
        NoReqBody
        jsonResponse
        ("key" =: key <> "token" =: token)
    liftIO $ print (responseBody r :: Card) -- CHANGED
```

Not bad, huh?

```bash
./recur.hs
# Card {name = "Sun | Send PinkLetter"}
```

### Gimme the Labels

Easy, peasy. Add a field...

```hs
data Card = Card
  { name :: Text,
    labels :: [Label] -- CHANGED
  }
  deriving (Generic, Show)
```

...define the type...

```hs
data Label = Label
  { id :: Text,
    name :: Text
  }
  deriving (Generic, Show)

instance FromJSON Label
```

...done! Haskell is awesome.

Wait, wat?

```bash
./recur.hs
    Multiple declarations of 'name'
   |
28 |   { name :: Text
   |     ^^^^
```

It turns out, [records in Haskell are a pain in the dot](https://odone.io/posts/2020-06-01-records-haskell.html). We could use some type-trickery, but also no:

```hs
data Card = Card
  { cardName :: Text, -- CHANGED
    cardLabels :: [Label] -- CHANGED
  }

data Label = Label
  { labelId :: Text, -- CHANGED
    labelName :: Text -- CHANGED
  }
```

Unfortunately, this means we cannot rely on generic deriving anymore. Bad kiddo Haskell!

```hs
instance FromJSON Card where
  parseJSON = withObject "Card" $ \v ->
    Card
      <$> v .: "name"
      <*> v .: "labels"


instance FromJSON Label where
  parseJSON = withObject "Label" $ \v ->
    Label
      <$> v .: "id"
      <*> v .: "name"
```

And we are back on track:

```bash
./recur.hs
# Card {cardName = "Sun | Send PinkLetter", cardLabels = [Label {labelId = "...", labelName = "Craft"}]}
```

### GET a List of Cards

Oh my, this is going to be a hell of a change. Naaaa:

```hs
httpGet :: IO ()
httpGet =
  runReq defaultHttpConfig $ do
    r <-
      req
        GET
        (https "trello.com" /: "1" /: "list" /: listId /: "cards") -- CHANGED
        NoReqBody
        jsonResponse
        ("key" =: key <> "token" =: token)
    liftIO $ print (responseBody r :: [Card]) -- CHANGED
```

Take that Bash!

```bash
./recur.hs
# [Card {cardName = "Sun | Send PinkLetter", cardLabels = [Label {labelId = "...", labelName = "Craft"}]},Card {cardName = "Check Twitter stats", cardLabels = [Label {labelId = "...", labelName = "Community"}]},Card {cardName = "D15 | Check YouTube channel",cardLabels = [Label {labelId = "...", labelName = "Craft"},Label {labelId = "...", labelName = "Community"}]}]
```

### Gimme the Interval

We start by hardcoding it:

```hs
data Card = Card
  { cardName :: Text,
    cardLabels :: [Label],
    cardInterval :: Text -- CHANGED
  }

instance FromJSON Card where
  parseJSON = withObject "Card" $ \v ->
    Card
      <$> v .: "name"
      <*> v .: "labels"
      <*> pure "INTERVAL" -- CHANGED
```

The interval is part of the name. However, applicative does not allow to express a dependency between name and interval, but [monads can be sequenced](https://odone.io/posts/2020-02-03-monad-composes-sequentially.html).

Let's make the change easy...

```hs
instance FromJSON Card where
  parseJSON = withObject "Card" $ \v -> do
    name <- v .: "name"
    labels <- v .: "labels"
    interval <- pure "INTERVAL"
    pure $ Card name labels interval
```

...and then make the easy change:

```hs
instance FromJSON Card where
  parseJSON = withObject "Card" $ \v -> do
    name <- v .: "name"
    labels <- v .: "labels"
    let (interval, name') = breakOn " | " name
    pure $ Card name' labels interval
```

Et voilà:

```bash
./recur.hs
# [Card {cardName = "Send PinkLetter", cardLabels = [Label {labelId = "...", labelName = "Craft"}], cardInterval ="Sun"},Card {cardName = "Check Twitter stats", cardLabels = [Label {labelId = "...", labelName = "Community"}], cardInterval = "D01"},Card {cardName = "Check YouTube channel", cardLabels = [Label {labelId = "...", labelName = "Check YouTube channel"}], cardInterval = "D15"}]
```

### Interval Interval

Hold your horses!

We said we don't want to allow garbage in. What if a card is named "LOL | Invalid interval".

Instead of using `Text`, we can create a type:

```hs
data Interval = DayOfWeek DayOfWeek | DayOfMonth DayOfMonth
  deriving (Show)

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Generic, Show)

instance FromJSON DayOfWeek

data DayOfMonth
  = D01
  | D02
  | D03
  | D04
  | ...
  deriving (Generic, Show)

instance FromJSON DayOfMonth
```

Tweak a couple of things:

```hs
data Card = Card
  { cardName :: Text,
    cardLabels :: [Label],
    cardInterval :: Interval -- CHANGED
  }

instance FromJSON Card where
  parseJSON = withObject "Card" $ \v -> do
    (interval, name) <- (v .: "name") >>= parseName -- CHANGED
    labels <- v .: "labels"
    pure $ Card name labels interval
```

Then, we define `parseName`; notice that we wrap the JSON string into an aeson String to make things type-check and re-use as much library code as possible.

I'm sure there was a better way, but screw it: it's just a script, right?

```hs
instance FromJSON Interval where
  parseJSON = withText "Interval" $ \v ->
--            ^ Expected a JSON string that is either...
    (DayOfWeek <$> parseJSON (String v)) <|> (DayOfMonth <$> parseJSON (String v))
--                 ^ ...a DayOfWeek parsed with its generic derived parser...
--                                        ^ ...or...
--                                                           ^ ...a DayOfMonth.

parseName :: Text -> Parser (Interval, Text)
parseName name = do
  let (candidateInterval, finalName) = breakOn " | " name
  interval <- parseJSON . String $ candidateInterval
  pure (interval, finalName)
```

Rock and roll:

```bash
./recur.hs
# [Card {cardName = "Send PinkLetter", cardLabels = [Label {labelId = "...", labelName = "Craft"}], cardInterval = DayOfWeek Sun},Card {cardName = "Check Twitter stats", cardLabels = [Label {labelId = "...", labelName = "Community"}], cardInterval = DayOfMonth D01},Card {cardName = "Check YouTube channel", cardLabels = [Label {labelId = "...", labelName = "Craft"},Label {labelId = "...", labelName = "Community"}], cardInterval = DayOfMonth D15}]
```


### POST a Card

Excellent, we can fetch cards from Trello, which is as useful as a waterproof towel. It's time to make things appear on the board.

Given the laws of Haskell, the following code compiles:

```hs
main :: IO ()
main = do
  cards <- httpGet
  traverse_ print cards
```

Therefore, we need to make this work:

```hs
main :: IO ()
main = do
  cards <- httpGet
  traverse_ httpPost cards

httpPost :: Card -> IO ()
httpPost card =
  -- ...
```

Preparing the query params for name and labels is straightforward; the due date not so much. But we will solve the latter later:

```hs
idTargetList :: Text
idTargetList = "..."

httpPost :: Card -> IO ()
httpPost card =
  runReq defaultHttpConfig $ do
    let idLabels = intercalate "," (fmap labelId . cardLabels $ card)
    let name = cardName card
    let due = "2020-12-30T16:10:00Z" :: Text
    r <-
      req
        POST
        (https "trello.com" /: "1" /: "cards")
        NoReqBody
        jsonResponse
        ("key" =: key <> "token" =: token <> "idList" =: idTargetList <> "name" =: name)
    liftIO $ print (responseBody r :: Value)
```

Time to show off a bit. Since `(=:)` builds an `Option` that has a `Monoid` instance, we can refactor to:

```hs
fold
  [ "key" =: key,
    "token" =: token,
    "idList" =: idTargetList,
    "name" =: name,
    "idLabels" =: idLabels,
    "due" =: due
  ]
```

### Gotta Due it

We are left with two problems: creating only cards that are due in the next seven days and transforming the interval into a due date.

We could throw all that logic inside `httpPost`. But we are functional programmers, and we swear by pipelines.

Also, we don't want to have two different types: `CardWithInterval` and `CardWithUTCTime`. It's a waste of characters, and it's bad for [my RSI](https://www.youtube.com/watch?v=gWrfm-Rrf2E).

We can be smart about it:

```hs
{-# LANGUAGE FlexibleInstances #-}

data Card due = Card -- CHANGED
  { cardName :: Text,
    cardLabels :: [Label],
    cardDue :: due -- CHANGED
  }

instance FromJSON (Card Interval) where -- CHANGED
-- ...

httpGet :: IO [Card Interval] -- CHANGED
httpGet =
  runReq defaultHttpConfig $ do
    r <-
      req
        GET
        (https "trello.com" /: "1" /: "list" /: idTemplatesList /: "cards")
        NoReqBody
        jsonResponse
        ("key" =: key <> "token" =: token)
    pure (responseBody r :: [Card Interval]) -- CHANGED

httpPost :: Card UTCTime -> IO () -- CHANGED
httpPost card =
  runReq defaultHttpConfig $ do
    let idLabels = intercalate "," (fmap labelId . cardLabels $ card)
    let name = cardName card
    let due = formatTime defaultTimeLocale "%Y-%m-%dT09:00:00Z" $ cardDue card -- CHANGED
```

In other words, `httpGet` always returns `Card Interval`s and `httpPost` only accepts a `Card UTCTime`. We managed to have one type, but guaranteed type-safety.

Now, we need a function to convert `Card Interval` to `Card UTCTime` and filter out cards that are not due next week:

```hs
main :: IO ()
main = do
  cards <- httpGet
  now <- getCurrentTime
  let week = take 7 . iterate (addUTCTime nominalDay) $ now
  let cards' = mapMaybe (addDueUTCTime week) cards
--             ^ Like fmap but discards Nothings.
  traverse_ httpPost cards'

addDueUTCTime :: [UTCTime] -> Card Interval -> Maybe (Card UTCTime)
--                                             ^ Returns Nothing when the Card is not due this week.
addDueUTCTime days candidatecard =
  (\day -> candidateCard {cardDue = day}) <$> find (dueOn candidateCard) days
--         ^ Replaces the Interval with UTCTime: Card Interval -> Card UTCTime.
  where
    dueOn (Card _ _ (DayOfMonth dom)) day =
      show dom == formatTime defaultTimeLocale "D%d" day
    dueOn (Card _ _ (DayOfWeek dow)) day =
      show dow == formatTime defaultTimeLocale "%a" day
```

Jackpot!11oneon1one!!

Let me run the script fifty times to celebrate!

```bash
Timeout: 429 error
```

### Rate Limit Yourself

Well, it may be better to respect the API rate limits:

```hs
httpGet :: IO [Card Interval]
httpGet =
  runReq defaultHttpConfig $ do
    r <- -- ...
    liftIO $ threadDelay 100000 -- CHANGED
    pure (responseBody r :: [Card Interval])

httpPost :: Card UTCTime -> IO ()
httpPost card =
  runReq defaultHttpConfig $ do
    r <- -- ...
    liftIO $ threadDelay 100000 -- CHANGED
    liftIO $ print (responseBody r :: Value)
```

Now we can `while true` the script.

Hmm, wait, how do you do that in Bash?

...

Damn, I want to go back to Haskell already.

### Command Line Options

It wouldn't be a script without command-line options, would it? We can pass API token, API key, idTemplatesList, and idTargetList from the shell.

Mr. Nike said, "practice makes perfect", so why not copy-pasting one last time from [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)?

```hs
data CliOptions = CliOptions
  { apiToken :: Text,
    apiKey :: Text,
    idTemplatesList :: Text,
    idTargetList :: Text
  }
  deriving (Show)

optionsParser :: Parser CliOptions
optionsParser =
  CliOptions
    <$> strOption
      ( long "api-token"
          <> metavar "API_TOKEN"
          <> help "Trello API token"
      )
    <*> strOption
      ( long "api-key"
          <> metavar "API_KEY"
          <> help "Trello API key"
      )
    <*> strOption
      ( long "templates-list-id"
          <> metavar "TEMPLATES_LIST_ID"
          <> help "Id of the Trello list containing the recurring template cards"
      )
    <*> strOption
      ( long "target-list-id"
          <> metavar "TARGET_LIST_ID"
          <> help "Id of the Trello list where to create the cards"
      )

main :: IO ()
main = do
  createCards =<< execParser options
  where
    options =
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc
              "Creates cards in TARGET_LIST_ID by parsing recurring template cards from TEMPLATES_LIST_ID."
            <> footerDoc footer
            <> header "recur.hs - recurring cards for Trello"
        )
    footer = "..."

httpGet :: CliOptions -> IO [Card Interval] -- CHANGED
httpGet opts = -- CHANGED
  -- ...

httpPost :: CliOptions -> Card UTCTime -> IO () -- CHANGED
httpPost opts card = -- CHANGED
  -- ...
```

With that in place, we can ask for `--help`:

```bash
$ ./recur.hs --help

recur.hs - recurring cards for Trello

Usage: recur.hs --api-token API_TOKEN --api-key API_KEY
                --templates-list-id TEMPLATES_LIST_ID
                --target-list-id TARGET_LIST_ID
  Creates cards in TARGET_LIST_ID by parsing recurring template cards from
  TEMPLATES_LIST_ID.

Available options:
  --api-token API_TOKEN    Trello API token
  --api-key API_KEY        Trello API key
  --templates-list-id TEMPLATES_LIST_ID
                           Id of the Trello list containing the recurring
                           template cards
  --target-list-id TARGET_LIST_ID
                           Id of the Trello list where to create the cards
  -h,--help                Show this help text

Only the template cards that are due in the next 7 days (starting from today) are created.

cards in TEMPLATES_LIST_ID are expected to be named with the following convention:
  - "INTERVAL | NAME"
    - INTERVAL can be either
      - DXX for a specific day of the month
      - The first three letters of the day of the week (e.g. Mon)
    - NAME is whatever you want

The script creates only the cards that are due next week by comparing INTERVAL with the current system date.

NAME is copied as is, labels are preserved, and due date is generated out of INTERVAL.
```

## Outro

There's one issue when it comes to scripts vs. silly textbook exercises: shit gets real.

For example, this script does not take care of:

- Idempotency: running it multiple times on the same week would create duplicates.
- It only looks seven days in the future.
- It does not fail gracefully (e.g., parsing failure)

However, I hope this was enough to convince you to give Haskell scripting a go and get your cylinders fired up.

So should we rewrite all of our scripts in Haskell?

If you don't give a damn about portability, don't have colleagues complaining they don't have a Ph.D. in math—don't tell them about the Hello, World! trick–or compilation time, I would say hell yeah.

At first, it's strange to type-drive a script, but having Haskell's full power and the maintainability provided by types, is a game-changer.

Want to see more scripts? Smash [this link](https://odone.io/tags/Script.html)!

## The Whole Script

```hs
#!/usr/bin/env stack
{- stack
  script
  --resolver lts-16.25
  --package req
  --package aeson
  --package text
  --package time
  --package optparse-applicative
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Types (FromJSON, Parser, Value (..), parseJSON, withObject, withText, (.:))
import Data.Foldable (fold, traverse_)
import Data.List (find, intercalate)
import Data.Maybe (mapMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text, intercalate, splitOn)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime, nominalDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.Generics (Generic)
import Network.HTTP.Req (GET (..), NoReqBody (..), POST (..), Req, defaultHttpConfig, https, jsonResponse, req, responseBody, runReq, (/:), (=:))
import Options.Applicative (Parser, execParser, footerDoc, fullDesc, header, help, helper, info, long, metavar, progDesc, strOption, (<**>), (<|>))
import Options.Applicative.Help.Pretty (text)

data Card due = Card
  { cardName :: Text,
    cardLabels :: [Label],
    cardDue :: due
  }
  deriving (Generic, Show)

instance FromJSON (Card Interval) where
  parseJSON = withObject "Card" $ \v -> do
    (interval, name) <- (v .: "name") >>= parseName
    labels <- v .: "labels"
    pure $ Card name labels interval

parseName :: Text -> Data.Aeson.Types.Parser (Interval, Text)
parseName name = do
  let [candidateInterval, finalName] = splitOn " | " name
  interval <- parseJSON . String $ candidateInterval
  pure (interval, finalName)

data Label = Label
  { labelId :: Text,
    labelName :: Text
  }
  deriving (Generic, Show)

instance FromJSON Label where
  parseJSON = withObject "Label" $ \v ->
    Label <$> v .: "id" <*> v .: "name"

data Interval = DayOfWeek DayOfWeek | DayOfMonth DayOfMonth
  deriving (Generic, Show)

instance FromJSON Interval where
  parseJSON = withText "Interval" $ \v ->
    (DayOfWeek <$> parseJSON (String v)) <|> (DayOfMonth <$> parseJSON (String v))

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Generic, Show)

instance FromJSON DayOfWeek

data DayOfMonth
  = D01
  | D02
  | D03
  | D04
  | D05
  | D06
  | D07
  | D08
  | D09
  | D10
  | D11
  | D12
  | D13
  | D14
  | D15
  | D16
  | D17
  | D18
  | D19
  | D20
  | D21
  | D22
  | D23
  | D24
  | D25
  | D26
  | D27
  | D28
  | D29
  | D30
  | D31
  deriving (Generic, Show)

instance FromJSON DayOfMonth

data CliOptions = CliOptions
  { apiToken :: Text,
    apiKey :: Text,
    idTemplatesList :: Text,
    idTargetList :: Text
  }
  deriving (Show)

optionsParser :: Options.Applicative.Parser CliOptions
optionsParser =
  CliOptions
    <$> strOption
      ( long "api-token"
          <> metavar "API_TOKEN"
          <> help "Trello API token"
      )
    <*> strOption
      ( long "api-key"
          <> metavar "API_KEY"
          <> help "Trello API key"
      )
    <*> strOption
      ( long "templates-list-id"
          <> metavar "TEMPLATES_LIST_ID"
          <> help "Id of the Trello list containing the recurring template cards"
      )
    <*> strOption
      ( long "target-list-id"
          <> metavar "TARGET_LIST_ID"
          <> help "Id of the Trello list where to create the cards"
      )

main :: IO ()
main = do
  createCards =<< execParser options
  where
    options =
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc
              "Creates cards in TARGET_LIST_ID by parsing recurring template cards from TEMPLATES_LIST_ID."
            <> footerDoc footer
            <> header "hello - a test for optparse-applicative"
        )
    footer =
      Just . Options.Applicative.Help.Pretty.text . Data.List.intercalate "\n" $
        [ "Only the template cards that are due in the next 7 days (including today) are created.",
          "",
          "Cards in TEMPLATES_LIST_ID are expected to be named with the following convention:",
          "  - \"INTERVAL | NAME\"",
          "    - INTERVAL can be either",
          "      - DXX for a specific day of the month",
          "      - The first three letters of the day of the week (e.g. Mon)",
          "    - NAME is whatever you want",
          "",
          "The script creates only the cards that are due next week by comparing INTERVAL with the current system date.",
          "",
          "NAME is copied as is, labels are preserved, and due date is generated out of INTERVAL."
        ]

createCards :: CliOptions -> IO ()
createCards opts = do
  cards <- httpGet opts
  now <- getCurrentTime
  let week = Prelude.take 7 . iterate (addUTCTime nominalDay) $ now
  let cards' = mapMaybe (addDueUTCTime week) cards
  traverse_ (httpPost opts) cards'

addDueUTCTime :: [UTCTime] -> Card Interval -> Maybe (Card UTCTime)
addDueUTCTime days candidateCard =
  (\day -> candidateCard {cardDue = day}) <$> find (dueOn candidateCard) days
  where
    dueOn (Card _ _ (DayOfMonth dom)) day =
      show dom == formatTime defaultTimeLocale "D%d" day
    dueOn (Card _ _ (DayOfWeek dow)) day =
      show dow == formatTime defaultTimeLocale "%a" day

httpGet :: CliOptions -> IO [Card Interval]
httpGet opts =
  runReq defaultHttpConfig $ do
    r <-
      req
        GET
        (https "trello.com" /: "1" /: "list" /: idTemplatesList opts /: "cards")
        NoReqBody
        jsonResponse
        ("key" =: apiKey opts <> "token" =: apiToken opts)
    liftIO $ threadDelay 100000
    pure (responseBody r :: [Card Interval])

httpPost :: CliOptions -> Card UTCTime -> IO ()
httpPost opts card =
  runReq defaultHttpConfig $ do
    let idLabels = Data.Text.intercalate "," (fmap labelId . cardLabels $ card)
    let name = cardName card
    let due = formatTime defaultTimeLocale "%Y-%m-%dT09:00:00Z" $ cardDue card
    r <-
      req
        POST
        (https "trello.com" /: "1" /: "cards")
        NoReqBody
        jsonResponse
        ( fold
            [ "key" =: apiKey opts,
              "token" =: apiToken opts,
              "idList" =: idTargetList opts,
              "name" =: name,
              "idLabels" =: idLabels,
              "due" =: due
            ]
        )
    liftIO $ threadDelay 100000
    liftIO $ print (responseBody r :: Value)
```

Thanks [Advent of Haskell](https://adventofhaskell.com) for featuring this article.
