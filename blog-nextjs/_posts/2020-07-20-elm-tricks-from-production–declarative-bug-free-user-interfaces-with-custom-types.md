---
title: Elm Tricks from Production–Declarative, Bug-Free User Interfaces with Custom Types
description: How to use custom types to keep the interface declarative and avoid bugs.
published: true
author: Riccardo
cover_image: /images/elm.jpg
canonical_url: https://blog.lunarlogic.io/2019/elm-tricks-from-production-custom-types/
series: Elm Tricks from Production
tags:
  - Functional Programming
  - Elm
---

There are 4 possible statuses when it comes to fetching data in a frontend application:

- before fetching
- while fetching
- successful fetch
- failed fetch

In JavaScript it's easy to either forget handling some statuses or to declare cleanly what to render on screen for each status. This opens the gates to a lot of bugs. Ever been on a page with a spinner that never disappears?

Elm solves both the problems defined above in perfect functional-programming style, adding a type. In particular, let's say we want to create a new cool webapp. It will show a random number every time the page is refreshed, cool right?! We can use a public API to fetch the random number. Also, we need to choose a type, let’s go with `Int`. Unfortunately, the random number API is really slow so we need to show a message while fetching. We could use 0 to mark the fact that we still don't have the random number:

```elm
case number of
  0 -> Html.text "Loading..."
  i -> Html.text ("The random number is: " ++ String.fromInt i)
```

But this is not a good solution: if the API returns 0 as a random number we will be showing the loading message forever! Let's try with `Maybe Int`:

```elm
case maybeNumber of
  Nothing -> Html.text "Loading..."
  Just i  -> Html.text ("The random number is: " ++ String.fromInt i)
```

Cool, now we can distinguish between not having the number or having it. What if we got an error though? Let's add a flag for that:

```elm
case (maybeNumber, hasFailed) of
  (Nothing, False) -> Html.text "Loading..."
  (Just i, False)  -> Html.text ("The random number is: " ++ String.fromInt i) 
  (Nothing, True)  -> Html.text "Could not fetch random number" 
  (Just i, True)   -> WTH??
```

Problem is there's a combination that does not make sense at all (i.e. `(Just i, True)`): we fetched a number but there's an error. Custom type to the rescue:

```elm
type RemoteData
  = Loading
  | Success Int
  | Failure

case remoteDataNumber of
  Loading   -> Html.text "Loading..."
  Success i -> Html.text ("The random number is: " ++ String.fromInt i) 
  Failure   -> Html.text "Could not fetch random number"
```

Now, let's say that instead of loading the random number every time the page is refreshed, we want to start fetching as soon as a button is clicked:

```elm
type RemoteData
  = NotAsked
  | Loading
  | Success Int
  | Failure

case remoteDataNumber of
  NotAsked  -> Html.text "Click the button!"
  Loading   -> Html.text "Loading..."
  Success i -> Html.text ("The random number is: " ++ String.fromInt i) 
  Failure -> Html.text "Could not fetch random number"
```

The beauty of this solution is that, as soon as we declare our random number to be of `RemoteData` type, the Elm compiler will enforce checking all the branches. Also, it's really easy to see what the app is doing depending on the status of the request. 
 
[AirCasting](http://aircasting.org/) uses the pattern shown above extensively. In particular, to avoid reinventing the wheel we used [RemoteData](https://package.elm-lang.org/packages/krisajenkins/remotedata/latest/RemoteData). One example is the [function](https://github.com/HabitatMap/AirCasting/blob/5d87477dc26525d9049bdcb1681e82021c6b28a9/app/javascript/elm/src/Main.elm#L1114) that takes care of rendering either the sessions list or the selected session at the bottom of the [map page](http://aircasting.org/mobile_map):

```elm
viewSessionsOrSelectedSession selectedSession =
        div []
            [ case selectedSession of
                NotAsked ->
                    viewSessions

                Success session ->
                    viewSelectedSession (Just session)

                Loading ->
                    viewSelectedSession Nothing

                Failure _ ->
                    div [] [ text "error!" ]
            ]
```

In other words, if the request for the selected session is not ongoing, show all the sessions. If the selected session is loading show its view while waiting for the data to come. If the request was successful show the selected session view for it. If the request failed show the error message.

If you are not using Elm you can still [apply the pattern successfully](https://www.youtube.com/watch?v=TjxicgU0rQQ). Unfortunately, there won’t be a nice compiler helping guaranteeing an error free application 😉.
