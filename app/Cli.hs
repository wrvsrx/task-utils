{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Cli (
  VisOption (..),
  TotalOption (..),
  EventOption (..),
  ModOption (..),
  totalParser,
) where

import Data.Text qualified as T
import Data.Time (
  LocalTime (..),
  defaultTimeLocale,
  parseTimeM,
 )
import Options.Applicative
import Task (TaskDate (..))

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

dateParser :: Parser TaskDate
dateParser = argument dayReader (metavar "DATE" <> value (RelativeDate 0))
 where
  parseRelativeDate :: String -> Maybe TaskDate
  parseRelativeDate arg = do
    case arg of
      "today" -> Just $ RelativeDate 0
      "tomorrow" -> Just $ RelativeDate 1
      "yesterday" -> Just $ RelativeDate (-1)
      _ -> Nothing
  parseAbsoluteDate :: String -> Maybe TaskDate
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
  | ListEvent TaskDate
  | Mod ModOption
  | ListTask (Maybe T.Text)
  | PendingTask (Maybe T.Text)
  | FinishTask (Maybe T.Text)
  | ViewTask (Maybe T.Text)
  | AddTask [T.Text]
  | DeleteTask T.Text
  | VisualizeEvent VisualizeEventOption
  | -- shortcuts
    Date TaskDate
  | DateTag TaskDate

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

data TimeRange
  = TimeRangeDay TaskDate
  | TimeRangeRange LocalTime (Maybe LocalTime)
  deriving (Show)

data VisualizeEventOption = CliOption
  { calendarDir :: Maybe FilePath
  , timeRange :: Maybe TimeRange
  , outputPng :: FilePath
  , cacheJSONPath :: Maybe FilePath
  , configPath :: Maybe FilePath
  }
  deriving (Show)

calendarVisualizationParser :: Parser VisualizeEventOption
calendarVisualizationParser = do
  calendarDir <-
    optional $
      strOption
        ( long "calendar-dir"
            <> short 'c'
            <> help "calendar directory"
        )
  timeRange <- optional timeRangeParser
  outputPng :: FilePath <-
    strOption
      ( long "png"
          <> short 'p'
          <> help "output png file path"
          <> metavar "PNG_FILE"
      )
  cacheJSONPath <-
    optional $
      strOption
        ( long "cache"
            <> metavar "CACHE_JSON_PATH"
            <> help "cache json file path, default to $XDG_CACHE_HOME/task-utils/cache.json"
        )
  configPath <-
    optional $
      strOption
        ( long "config"
            <> help "configuration file, default to $XDG_CONFIG_HOME/task-utils/config.json"
            <> metavar "CONFIG"
        )
  pure
    CliOption
      { calendarDir = calendarDir
      , timeRange = timeRange
      , outputPng = outputPng
      , cacheJSONPath = cacheJSONPath
      , configPath = configPath
      }
 where
  dateTimeReader = maybeReader $ parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S" :: ReadM LocalTime
  startTimeParser = option dateTimeReader (long "start-time" <> short 's' <> help "start time")
  endTimeParser = option dateTimeReader (long "end-time" <> short 'e' <> help "end time")

  timeRangeParser =
    TimeRangeDay
      <$> dateParser
        <|> TimeRangeRange
      <$> startTimeParser
      <*> optional endTimeParser

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
        <> command "view-task" (info (ViewTask <$> maybeFilterParser) idm)
        <> command "add-task" (info (AddTask <$> many (argument str (metavar "TASK_INFO"))) idm)
        <> command "delete-task" (info (DeleteTask <$> filterParser) idm)
        <> command "date-tag" (info (DateTag <$> dateParser) idm)
        <> command "date" (info (Date <$> dateParser) idm)
        <> command "visualize-event" (info (VisualizeEvent <$> calendarVisualizationParser) idm)
    )
