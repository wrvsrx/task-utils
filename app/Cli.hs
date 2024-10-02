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
  , filter :: T.Text
  }

data EventOption = EventOption
  { summary :: T.Text
  , start :: T.Text
  , end :: T.Text
  , task :: Maybe T.Text
  }

data TotalOption
  = Closure T.Text
  | Vis VisOption
  | Event EventOption
  | Mod ModOption
  | ListTask T.Text
  | ListEvent (Maybe Day)
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
    <*> argument str (metavar "FILTER")

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

totalParser :: Parser TotalOption
totalParser =
  hsubparser
    ( command "visualize" (info (Vis <$> visParser) idm)
        <> command "closure" (info (Closure <$> argument str (metavar "FILTER")) idm)
        <> command "mod" (info (Mod <$> modParser) idm)
        <> command "date-tag" (info (DateTag <$> dateParser) idm)
        <> command "date" (info (Date <$> optional dateParser) idm)
        <> command "list-task" (info (ListTask <$> argument str (metavar "FILTER")) idm)
        <> command "add-event" (info (Event <$> eventParser) idm)
        <> command "list-event" (info (ListEvent <$> optional dateParser) idm)
    )
