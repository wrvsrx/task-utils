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
import Data.Time (Day (..), defaultTimeLocale, parseTimeM)
import Options.Applicative

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

data TotalOption
  = Closure (Maybe T.Text)
  | Vis VisOption
  | Event EventOption
  | ListEvent (Maybe Day)
  | Mod ModOption
  | ListTask (Maybe T.Text)
  | PendingTask (Maybe T.Text)
  | FinishTask (Maybe T.Text)
  | AddTask [T.Text]
  | -- shortcuts
    Date (Maybe Day)
  | DateTag Day

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
    <*> filterParser

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

dateParser :: Parser Day
dateParser = argument reader (metavar "DATE")
 where
  reader = eitherReader $ \arg ->
    case parseTimeM True defaultTimeLocale "%Y%m%d" arg <|> parseTimeM True defaultTimeLocale "%Y-%m-%d" arg of
      Just x -> Right x
      Nothing -> Left "Failed to parse date. The supported formats are YYYYMMDD or YYYY-MM-DD."

filterParser :: Parser (Maybe T.Text)
filterParser = optional (argument str (metavar "FILTER"))

totalParser :: Parser TotalOption
totalParser =
  hsubparser
    ( command "visualize" (info (Vis <$> visParser) idm)
        <> command "closure" (info (Closure <$> filterParser) idm)
        <> command "mod" (info (Mod <$> modParser) idm)
        <> command "add-event" (info (Event <$> eventParser) idm)
        <> command "list-event" (info (ListEvent <$> optional dateParser) idm)
        <> command "list-task" (info (ListTask <$> filterParser) idm)
        <> command "pending-task" (info (PendingTask <$> filterParser) idm)
        <> command "finish-task" (info (FinishTask <$> filterParser) idm)
        <> command "add-task" (info (AddTask <$> many (argument str (metavar "TASK_INFO"))) idm)
        <> command "date-tag" (info (DateTag <$> dateParser) idm)
        <> command "date" (info (Date <$> optional dateParser) idm)
    )
