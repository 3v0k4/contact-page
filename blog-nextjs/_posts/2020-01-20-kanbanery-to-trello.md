---
title: From Kanbanery to Trello
description: Using a Haskell script to migrate a Kanbanery board to Trello
author: Riccardo
tags:
  - Functional Programming
  - Haskell
  - Script
---

The scripting spree is not ended yet! To write the last few posts here I've fruitfully used ["Scaffolding a Blog Post"](/posts/2019-12-26-scaffolding-a-blog-post/), ["Tweeting a Blog Post via command line"](/posts/2020-01-06-posting-a-tweet-with-haskell/) and ["Crossposting to DevTo via command line"](/posts/2020-01-13-crossposting-via-command-line/). Now it's time to see a script that was written for a work-related task.

In particular, we wanted to move a few of our project kanban boards from [Kanbanery](https://kanbanery.com/) to [Trello](https://trello.com/). Since I wrote the script after reading the awesome ["Optics by Example"](https://leanpub.com/optics-by-example), the code uses optics.

Let's see how the script works first:

```bash
$ ./kan2tre.hs --help

Usage: kan2tre.hs [--in CSV_FILE] TRELLO_BOARD_ID TRELLO_API_KEY
                  TRELLO_API_TOKEN
  Moves all tickets exported from Kanbanery in CSV_FILE to the Trello board
  identified by TRELLO_BOARD_ID. Kanbanery exports a CSV with the following
  schema: Title,Owner email,Task type,Estimate,Priority,Description,Column
  name,Creator email,Created at,Subtasks,Comments Some records exported by
  Kanbanery could be malformed. When encountered they are printed to stdout so
  that you can manually add them. Also, any failed request to the Trello API is
  printed to stdout. Subtasks and comments are strings separated by semicolon.
  Unfortunately, there's no way to distinguish between a separarator and a
  semicolon in a comment / subtask. Thus, this script does not attempt to split
  comments / subtasks. In other words, comments and subtasks are always going to
  be one string each. This scripts ignores Owner email, Estimate, Priority,
  Creator email and Created at.

Available options:
  --in CSV_FILE            Path to csv file with exported tickets from
                           Kanbanery (default: "./kanbanery.csv")
  TRELLO_BOARD_ID          Trello board id. To get this visit
                           https://trello.com/b/ID/reports.json where ID is the
                           one you see in the URL of your board. For example, in
                           the following URL the ID is 'lPbIpQIl'
                           https://trello.com/b/lPbIpQIl/habitatmap
  TRELLO_API_KEY           Trello api key
  TRELLO_API_TOKEN         Trello api token
  -h,--help                Show this help text
```

Here's code:

```hs
#!/usr/bin/env stack
{- stack
  script
  --resolver nightly-2019-12-21
  --package wreq
  --package optparse-applicative
  --package aeson
  --package bytestring
  --package lens
  --package filepath
  --package time
  --package cassava
  --package text
  --package split
  --package lens-aeson
  --package containers
  --package mtl
  --package unbounded-delays
  --package transformers
  --package optparse-applicative
  --package http-client
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

import Network.Wreq
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Aeson hiding ((.:))
import GHC.Generics
import Data.ByteString
import Data.ByteString.Char8
import Control.Lens hiding ((.=))
import System.FilePath.Posix
import Data.Foldable
import Data.Time
import Data.Time.Format.ISO8601
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Text
import Data.List.Split
import Data.List
import Data.Aeson.Lens
import Data.Maybe
import Data.Map.Strict
import Control.Monad.State.Strict
import Data.Time.Clock.POSIX
import Control.Concurrent.Thread.Delay
import Control.Monad.Trans.Reader
import Control.Exception
import Network.HTTP.Client
import Data.Text.Lens

data KanbaneryTicket =
  KanbaneryTicket
    { _kanbaneryTicketTitle :: String
    , _kanbaneryTicketOwnerEmail :: String
    , _kanbaneryTicketTaskType :: String
    , _kanbaneryTicketEstimate :: Maybe Float
    , _kanbaneryTicketPriority :: Maybe Float
    , _kanbaneryTicketDescription :: String
    , _kanbaneryTicketColumnName :: String
    , _kanbaneryTicketCreatorEmail :: String
    , _kanbaneryTicketCreatedAt :: NominalDiffTime
    , _kanbaneryTicketSubtasks :: String
    , _kanbaneryTicketComments :: String
    } deriving (Show,Eq,Ord)
makeFields ''KanbaneryTicket
-- ^ Generate accessors for each field of the record.
--   That means we can later do stuff like
--   `kanbaneryTicket ^. title` to "view" the title
--   without having to worry about duplicate record fields.

data KanbaneryTicketParseResult
  = Ok KanbaneryTicket
  | Malformed String
  deriving (Show)
makePrisms ''KanbaneryTicketParseResult
-- ^ Generate a Prism for each constructor of a data type.
--   See later uses of `_Ok` or `_Malformed`.

instance FromNamedRecord KanbaneryTicketParseResult where
--       ^ Tell cassava how to convert a CSV record to KanbaneryTicketParseResult.
    parseNamedRecord x
      | Data.Foldable.length x == 11 = Ok <$> parsed
--      ^ When the CSV record has all the fields then..
--                                     ^ ..it's wellformed..
      | otherwise                    = pure . Malformed $ show x
--      ^ ..otherwise..
--                                     ^ ..it's malformed.
      where
        parsed =
          KanbaneryTicket <$>
            (x .: "Title") <*>
            (x .: "Owner email") <*>
            (x .: "Task type") <*>
            (x .: "Estimate") <*>
            (x .: "Priority") <*>
            (x .: "Description") <*>
            (x .: "Column name") <*>
            (x .: "Creator email") <*>
            fmap (utcTimeToPOSIXSeconds . zonedTimeToUTC) (x .: "Created at") <*>
            (x .: "Subtasks") <*>
            (x .: "Comments")

instance FromField ZonedTime where
  parseField s = do
    mzt <- iso8601ParseM <$> parseField s
    Data.Maybe.maybe mempty pure mzt

data Opts =
  Opts
    { _optsCsvFile :: String
    , _optsTrelloBoardId :: String
    , _optsTrelloApiKey :: String
    , _optsTrelloToken :: String
    } deriving (Show)
makeFields ''Opts

data Env
  = Env
    { _envTrelloBoardId :: String
    , _envTrelloApiKey :: String
    , _envTrelloToken :: String
    } deriving (Show)
makeFields ''Env

type App a = ReaderT Env IO a
--   ^ Our application runs in an environment where it can..
--           ^ ..read `Env` (i.e. board id, api key and Trello token)..
--                       ^ ..perform IO..
--                          ^ ..return a result of type a.

main :: IO ()
main = do
  o <- execParser opts
-- ^ Get command line input. More on this in the previous posts.
  let env = Env (o ^. trelloBoardId) (o ^. trelloApiKey) (o ^. trelloToken)
-- ^ Wrap command line input into `Env`.
  Control.Monad.Trans.Reader.runReaderT (run $ o ^. csvFile) env
-- ^ Apply `env` to `run`.
  where
    opts = info (parser <**> helper)
      (  fullDesc
      <> progDesc "Moves all tickets exported from Kanbanery in CSV_FILE to the Trello board identified by TRELLO_BOARD_ID. \
                  \ \
                  \Kanbanery exports a CSV with the following schema: \
                  \Title,Owner email,Task type,Estimate,Priority,Description,Column name,Creator email,Created at,Subtasks,Comments \
                  \ \
                  \Some records exported by Kanbanery could be malformed. When encountered they are printed to stdout \
                  \so that you can manually add them. \
                  \ \
                  \Also, any failed request to the Trello API is printed to stdout. \
                  \ \
                  \Subtasks and comments are strings separated by semicolon. \
                  \Unfortunately, there's no way to distinguish between a separarator and a semicolon in a comment / subtask. \
                  \Thus, this script does not attempt to split comments / subtasks. In other words, comments and subtasks \
                  \are always going to be one string each. \
                  \ \
                  \This scripts ignores Owner email, Estimate, Priority, Creator email and Created at."
      )

parser :: Options.Applicative.Parser Opts
parser = Opts
      <$> strOption
         (  long "in"
         <> metavar "CSV_FILE"
         <> help "Path to csv file with exported tickets from Kanbanery"
         <> value "./kanbanery.csv"
         <> showDefault
         )
      <*> Options.Applicative.argument str
         (  metavar "TRELLO_BOARD_ID"
         <> help "Trello board id. To get this visit https://trello.com/b/ID/reports.json \
                  \where ID is the one you see in the URL of your board. For example, in the \
                  \following URL the ID is 'lPbIpQIl' https://trello.com/b/lPbIpQIl/habitatmap"
         )
      <*> Options.Applicative.argument str
         (  metavar "TRELLO_API_KEY"
         <> help "Trello api key"
         )
      <*> Options.Applicative.argument str
         (  metavar "TRELLO_API_TOKEN"
         <> help "Trello api token"
         )

run :: String -> App ()
run csvFile = do
  csv <- liftIO $ Data.ByteString.Lazy.readFile csvFile
--                ^ Read the CSV file.
--       ^ We need to liftIO because we are in the `App` monad transformer with IO as the base.
  case decodeByName csv of
    Left err -> liftIO . error . show $ err
--  ^ If the content of the file cannot be decoded by cassava then exit.
    Right (_,records) -> do
      let malformed = records ^.. folded . _Malformed
--                    ^ Take all the `Malformed` records and..
      liftIO $ Prelude.putStrLn $ "Found " <> (show . Data.Foldable.length $ malformed) <> " malformed records."
      traverse_ (liftIO . Prelude.putStrLn . (<>) "MALFORMED: ") malformed
--    ^ ..print some info about them so that they can be manually transferred to Trello.
      let kanbaneryTickets = records ^.. folded . _Ok
--                           ^ Take all the wellformed (i.e. `Ok`) records and..
      liftIO $ Prelude.putStrLn $ "Creating " <> (show . Data.Foldable.length $ kanbaneryTickets) <> " tickets."
      liftIO $ Prelude.putStrLn "Creating lists."
      columnNamesByListIds <- createLists kanbaneryTickets
--    ^ ..create in Trello the same columns that are on Kanbanery..
      liftIO $ Prelude.putStrLn "" >> Prelude.putStrLn "Creating cards."
      kanbaneryTicketsByCardId <- createCards columnNamesByListIds kanbaneryTickets
--    ^ ..create in Trello the tickets that are on Kanbanery..
      liftIO $ Prelude.putStrLn "" >> Prelude.putStrLn "Creating labels."
      labelsByIds <- createLabels kanbaneryTickets
--    ^ ..create in Trello the labels that are on Kanbanery..
      liftIO $ Prelude.putStrLn "" >> Prelude.putStrLn "Adding labels to tickets."
      traverse_ (addLabelsReq labelsByIds kanbaneryTicketsByCardId) kanbaneryTickets
--    ^ ..associate labels to tickets in Trello..
      liftIO $ Prelude.putStrLn "" >> Prelude.putStrLn "Adding subtasks to tickets."
      traverse_ (addSubtasksReq kanbaneryTicketsByCardId) kanbaneryTickets
--    ^ ..add subtasks that were in Kanbanery tickets to Trello tickets..
      liftIO $ Prelude.putStrLn "" >> Prelude.putStrLn "Adding creator email and creation date to tickets."
      traverse_ (addCreationReq kanbaneryTicketsByCardId) kanbaneryTickets
--    ^ ..for each ticket add the Kanbanery creation date as a comment..
      liftIO $ Prelude.putStrLn "" >> Prelude.putStrLn "Adding comments to tickets."
      traverse_ (addCommentsReq kanbaneryTicketsByCardId) kanbaneryTickets
--    ^ ..add comments that were in Kanbanery tickets to Trello tickets.
      liftIO $ Prelude.putStrLn "" >> Prelude.putStrLn "Done!"

createLists :: [KanbaneryTicket] -> App (Map String String)
createLists kanbaneryTickets = do
  let columnNames = Data.List.nub . Data.List.sort . toListOf (folded.columnName) $ kanbaneryTickets
--                                                   ^ From each ticket extract `columnName`.
  Data.Map.Strict.fromList . Data.Maybe.catMaybes <$> traverse f columnNames
  where
    f columnName = do
--  ^ Create column / list in Trello.
      mId <- createListReq columnName
      case mId of
        Just id -> pure $ Just (columnName,id)
--      ^ When successful..
--                 ^ ..return `columnName` and its Trello id.
        Nothing -> pure Nothing

createListReq :: String -> App (Maybe String)
createListReq list = do
  env <- Control.Monad.Trans.Reader.ask
  let opts = defaults & Network.Wreq.param "name"    .~ [Data.Text.pack list]
                      & Network.Wreq.param "idBoard" .~ [env ^. trelloBoardId . packed]
--                                                       ^ Using optics..
--                                                              ^ ..extract `trelloBoardId` and..
--                                                                              ^ ..transform to Text.
  postReq opts "https://api.trello.com/1/lists"

createCards :: Map String String -> [KanbaneryTicket] -> App (Map KanbaneryTicket String)
createCards columnNamesByListIds kanbaneryTickets =
  Data.Map.Strict.fromList . Data.Maybe.catMaybes <$> traverse f kanbaneryTickets
  where
    f kanbaneryTicket = do
--  ^ Create ticket in Trello.
      let listId = columnNamesByListIds ^?! ix (kanbaneryTicket ^. columnName)
--                 ^ Using optics..
--                                      ^ ..extract **UNSAFELY**..
--                                          ^ ..the value from the Map at the key..
--                                             ^ ..`columnName`.
      mId <- createCardReq listId (kanbaneryTicket ^. title) (kanbaneryTicket ^. description)
      case mId of
        Just id -> pure $ Just (kanbaneryTicket,id)
        Nothing -> pure Nothing

createCardReq :: String -> String -> String -> App (Maybe String)
createCardReq idList title description = do
  let opts = defaults & Network.Wreq.param "name"   .~ [Data.Text.pack title]
                      & Network.Wreq.param "desc"   .~ [Data.Text.pack description]
                      & Network.Wreq.param "idList" .~ [Data.Text.pack idList]
  postReq opts "https://api.trello.com/1/cards"

createLabels :: [KanbaneryTicket] -> App (Map String String)
createLabels kanbaneryTickets = do
  let taskTypes = Data.List.nub . Data.List.sort . toListOf (folded.taskType) $ kanbaneryTickets
  let colors = Data.List.cycle ["yellow", "purple", "blue", "red", "green", "orange", "black", "sky", "pink", "lime"]
  Data.Map.Strict.fromList . Data.Maybe.catMaybes <$> traverse f (Data.List.zip taskTypes colors)
  where
    f (label,color) = do
      mId <- createLabelReq label color
      case mId of
        Just id -> pure $ Just (label,id)
        Nothing -> pure Nothing

createLabelReq :: String -> String -> App (Maybe String)
createLabelReq label color = do
  env <- Control.Monad.Trans.Reader.ask
  let opts = defaults & Network.Wreq.param "name"    .~ [Data.Text.pack label]
                      & Network.Wreq.param "color"   .~ [Data.Text.pack color]
                      & Network.Wreq.param "idBoard" .~ [env ^. trelloBoardId . packed]
  postReq opts "https://api.trello.com/1/labels"

addLabelsReq :: Map String String -> Map KanbaneryTicket String -> KanbaneryTicket -> App ()
addLabelsReq labels tickets kanbaneryTicket =
  case labels ^? ix (kanbaneryTicket ^. taskType) of
    Just name -> do
      let opts = defaults & Network.Wreq.param "value" .~ [Data.Text.pack name]
      case tickets ^? ix kanbaneryTicket of
        Just id -> do
          postReq opts ("https://api.trello.com/1/cards/" <> id <> "/idLabels")
          pure ()
        Nothing ->
          pure ()
    Nothing ->
      pure ()

addSubtasksReq :: Map KanbaneryTicket String -> KanbaneryTicket -> App ()
addSubtasksReq tickets kanbaneryTicket =
  when (has folded $ kanbaneryTicket ^. subtasks) $
-- ^ When there's at least on subtask..
    case tickets ^? ix kanbaneryTicket of
      Just cardId -> do
        let opts = defaults & Network.Wreq.param "idCard" .~ [Data.Text.pack cardId]
        mCheckId <- postReq opts "https://api.trello.com/1/checklists"
--                  ^ ..create a Trello checklist and..
        case mCheckId of
          Just checkId -> do
            let opts2 = defaults & Network.Wreq.param "name" .~ [kanbaneryTicket ^. subtasks . packed]
            postReq opts2 ("https://api.trello.com/1/checklists/" <> checkId <> "/checkItems")
--          ^ ..add all the subtasks.
            pure ()
          Nothing -> pure ()
      Nothing -> pure ()

addCreationReq :: Map KanbaneryTicket String -> KanbaneryTicket -> App ()
addCreationReq tickets kanbaneryTicket =
  case tickets ^? ix kanbaneryTicket of
    Just id -> do
      let creator = kanbaneryTicket ^. creatorEmail
      let date = formatTime defaultTimeLocale "%F" . posixSecondsToUTCTime . view createdAt $ kanbaneryTicket
      let opts = defaults & Network.Wreq.param "text" .~ [Data.Text.pack $ "Created by " <> creator <> " on " <> date]
      postReq opts ("https://api.trello.com/1/cards/" <> id <> "/actions/comments")
      pure ()
    Nothing ->
      pure ()

addCommentsReq :: Map KanbaneryTicket String -> KanbaneryTicket -> App ()
addCommentsReq tickets kanbaneryTicket =
  when (has folded $ kanbaneryTicket ^. comments) $
    case tickets ^? ix kanbaneryTicket of
      Just id -> do
        let opts = defaults & Network.Wreq.param "text" .~ [kanbaneryTicket ^. comments . packed]
        postReq opts ("https://api.trello.com/1/cards/" <> id <> "/actions/comments")
        pure ()
      Nothing ->
        pure ()

postReq :: Network.Wreq.Options -> String -> App (Maybe String)
postReq opts url = do
  env <- Control.Monad.Trans.Reader.ask
  let opts' = opts & Network.Wreq.param "key"     .~ [env ^. trelloApiKey . packed]
                   & Network.Wreq.param "token"   .~ [env ^. trelloToken . packed]
                   & Network.Wreq.header "Accept" .~ ["application/json"]
                   & Network.Wreq.header "Content-Type" .~ ["application/json"]
  liftIO $ delay 35000
-- ^ Throttle requests to respect Trello API rate limits.
  liftIO $ Control.Exception.handle (handler opts') $ do
--                                   ^ In case of exception run `handler`.
    r <- liftIO $ Network.Wreq.postWith opts' url ("" :: ByteString)
    liftIO $ Prelude.putStr "."
    let resourceId = r ^?! Network.Wreq.responseBody .
                           Data.Aeson.Lens.key "id" .
                           Data.Aeson.Lens._String .
                           unpacked
    pure . Just $ resourceId
  where
    handler :: Network.Wreq.Options -> HttpException -> IO (Maybe String)
    handler opts' _ = do
--  ^ In case of exception log it and return failure (i.e. Nothing).
      liftIO . Prelude.putStrLn $ "Failed POST to " <> url
      liftIO . print $ opts'
      pure Nothing
```
