{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Cli (
  VisOption (..),
  TotalOption (..),
  EventOption (..),
  ModOption (..),
  totalParser,
) where

import Data.Text qualified as T
import Data.Time (defaultTimeLocale, parseTimeM)
import Options.Applicative
import TaskUtils (Date (..))

data VisOption = VisOption
  { highlights :: [T.Text]
  , deleted :: Bool
  , filter :: Maybe T.Text
  }

data EventOption = EventOption
  { summary :: T.Text
  , start :: T.Text
  , end :: T.Text
  , task :: Maybe T.Text
  }

dateParser :: Parser Date
dateParser = argument dayReader (metavar "DATE" <> value (RelativeDate 0))
 where
  parseRelativeDate :: String -> Maybe Date
  parseRelativeDate arg = do
    case arg of
      "today" -> Just $ RelativeDate 0
      "tomorrow" -> Just $ RelativeDate 1
      "yesterday" -> Just $ RelativeDate (-1)
      _ -> Nothing
  parseAbsoluteDate :: String -> Maybe Date
  parseAbsoluteDate arg = do
    case parseTimeM True defaultTimeLocale "%Y%m%d" arg <|> parseTimeM True defaultTimeLocale "%Y-%m-%d" arg of
      Just x -> Just $ AbsoluteDate x
      Nothing -> Nothing
  dayReader = eitherReader $ \arg ->
    case parseRelativeDate arg <|> parseAbsoluteDate arg of
      Just x -> Right x
      Nothing -> Left "Failed to parse date. The supported formats are `today`, `tomorrow`, `yesterday`, YYYYMMDD or YYYY-MM-DD."

data TotalOption
  = Closure (Maybe T.Text)
  | Vis VisOption
  | Event EventOption
  | ListEvent Date
  | Mod ModOption
  | ListTask (Maybe T.Text)
  | PendingTask (Maybe T.Text)
  | FinishTask (Maybe T.Text)
  | AddTask [T.Text]
  | DeleteTask T.Text
  | -- shortcuts
    Date Date
  | DateTag Date

data ModOption = ModOption
  { filter :: T.Text
  , modifiers :: [T.Text]
  }

parseHighlights :: String -> Either String [T.Text]
parseHighlights = Right . T.splitOn "," . T.pack

visParser :: Parser VisOption
visParser =
  VisOption
    <$> option
      (eitherReader parseHighlights)
      ( long "highlights"
          <> short 'h'
          <> value []
          <> help "Tags to be highlighted."
      )
    <*> flag False True (long "deleted" <> short 'd' <> help "Show deleted tasks.")
    <*> maybeFilterParser

modParser :: Parser ModOption
modParser =
  ModOption
    <$> argument str (metavar "FILTER")
    <*> many (argument str (metavar "MODIFIER"))

eventParser :: Parser EventOption
eventParser =
  EventOption
    <$> argument str (metavar "SUMMARY")
    <*> argument str (metavar "START")
    <*> argument str (metavar "END")
    <*> optional (argument str (metavar "TASK"))

filterParser :: Parser T.Text
filterParser = argument str (metavar "FILTER")

maybeFilterParser :: Parser (Maybe T.Text)
maybeFilterParser = optional filterParser

totalParser :: Parser TotalOption
totalParser =
  hsubparser
    ( command "visualize" (info (Vis <$> visParser) idm)
        <> command "closure" (info (Closure <$> maybeFilterParser) idm)
        <> command "modify-task" (info (Mod <$> modParser) idm)
        <> command "add-event" (info (Event <$> eventParser) idm)
        <> command "list-event" (info (ListEvent <$> dateParser) idm)
        <> command "list-task" (info (ListTask <$> maybeFilterParser) idm)
        <> command "pending-task" (info (PendingTask <$> maybeFilterParser) idm)
        <> command "finish-task" (info (FinishTask <$> maybeFilterParser) idm)
        <> command "add-task" (info (AddTask <$> many (argument str (metavar "TASK_INFO"))) idm)
        <> command "delete-task" (info (DeleteTask <$> filterParser) idm)
        <> command "date-tag" (info (DateTag <$> dateParser) idm)
        <> command "date" (info (Date <$> dateParser) idm)
    )
