{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Cli (
  VisOption (..),
  TotalOption (..),
  AddEventOption (..),
  ModOption (..),
  totalParser,
  parseVisualizeEventCliOption,
  EnvInfo (..),
  getEnvInfo,
) where

import Control.Monad (msum)
import Data.Aeson qualified as A
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Time (
  Day (..),
  LocalTime (..),
  TimeOfDay (..),
  TimeZone (..),
  UTCTime (..),
  defaultTimeLocale,
  getCurrentTime,
  getCurrentTimeZone,
  parseTimeM,
  utcToLocalTime,
 )
import Event (
  CalendarSummaryOption (..),
  ConfigFromFile (..),
 )
import Options.Applicative
import System.Directory (XdgDirectory (..), getXdgDirectory)
import Utils (TimeRange (..))

data EnvInfo = EnvInfo
  { currentTimeZone :: TimeZone
  , currentTime :: UTCTime
  , defaultConfigDirectory :: FilePath
  }

getEnvInfo :: IO EnvInfo
getEnvInfo = do
  currentTimeZone <- getCurrentTimeZone
  currentTime <- getCurrentTime
  defaultConfigDirectory <- getXdgDirectory XdgConfig "task-utils"
  return
    EnvInfo
      { currentTimeZone = currentTimeZone
      , currentTime = currentTime
      , defaultConfigDirectory = defaultConfigDirectory
      }

getToday :: EnvInfo -> Day
getToday envInfo = utcToLocalTime envInfo.currentTimeZone envInfo.currentTime & localDay

data VisOption = VisOption
  { highlights :: [T.Text]
  , deleted :: Bool
  , filter :: Maybe T.Text
  }

data AddEventOption = AddEventOption
  { summary :: T.Text
  , start :: LocalTime
  , end :: LocalTime
  , task :: Maybe T.Text
  }

dateStrings :: [String]
dateStrings = ["%Y-%m-%d", "%Y%m%d"]

readDateMaybe :: EnvInfo -> String -> Maybe Day
readDateMaybe envInfo arg =
  relativeDate <|> absoluteDate
 where
  currentDay = (utcToLocalTime envInfo.currentTimeZone envInfo.currentTime).localDay
  relativeDate :: Maybe Day
  relativeDate = do
    case arg of
      "today" -> Just currentDay
      "tomorrow" -> Just (succ currentDay)
      "yesterday" -> Just (pred currentDay)
      _ -> Nothing
  absoluteDate :: Maybe Day
  absoluteDate = msum $ map (\pattern -> parseTimeM False defaultTimeLocale pattern arg) dateStrings

timeStrings :: [String]
timeStrings = ["%H:%M:%S", "%H%M%S", "%H:%M", "%H%M"]

readTimeMaybe :: String -> Maybe TimeOfDay
readTimeMaybe arg = msum $ map (\pattern -> parseTimeM False defaultTimeLocale pattern arg) timeStrings

readDateTimeMaybe :: EnvInfo -> String -> Maybe LocalTime
readDateTimeMaybe envInfo arg = do
  msum [dateMaybe', timeMaybe, dateTimeMaybe]
 where
  today = getToday envInfo
  -- only date
  dateMaybe' = (\x -> LocalTime x (TimeOfDay 0 0 0)) <$> readDateMaybe envInfo arg
  -- only time
  timeMaybe = LocalTime today <$> readTimeMaybe arg
  -- time and date
  dateTimeMaybe = msum $ map (\pattern -> parseTimeM False defaultTimeLocale pattern arg) ((\x y -> x <> "T" <> y) <$> dateStrings <*> timeStrings)

dateReader :: EnvInfo -> ReadM Day
dateReader envInfo = eitherReader $ \arg ->
  case readDateMaybe envInfo arg of
    Just x -> Right x
    Nothing -> Left "Failed to parse date."

dateTimeReader :: EnvInfo -> ReadM LocalTime
dateTimeReader envInfo = eitherReader $ \arg ->
  case readDateTimeMaybe envInfo arg of
    Just x -> Right x
    Nothing -> Left "Failed to parse date time."

data TotalOption
  = GetTaskClosure (Maybe T.Text)
  | VisualizeTask VisOption
  | AddEvent AddEventOption
  | ListEvent Day
  | Mod ModOption
  | ListTask (Maybe T.Text)
  | PendingTask (Maybe T.Text)
  | FinishTask (Maybe T.Text)
  | ViewTask (Maybe T.Text)
  | AddTask [T.Text]
  | DeleteTask T.Text
  | VisualizeEvent VisualizeEventOption

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

addEventParser :: EnvInfo -> Parser AddEventOption
addEventParser envInfo =
  AddEventOption
    <$> argument str (metavar "SUMMARY")
    <*> argument (dateTimeReader envInfo) (metavar "START")
    <*> argument (dateTimeReader envInfo) (metavar "END")
    <*> optional (argument str (metavar "TASK"))

filterParser :: Parser T.Text
filterParser = argument str (metavar "FILTER")

maybeFilterParser :: Parser (Maybe T.Text)
maybeFilterParser = optional filterParser

data VisualizeEventOption = CliOption
  { calendarDir :: Maybe FilePath
  , timeRange :: Maybe TimeRange
  , outputPng :: FilePath
  , cacheJSONPath :: Maybe FilePath
  , configPath :: Maybe FilePath
  }
  deriving (Show)

calendarVisualizationParser :: EnvInfo -> Parser VisualizeEventOption
calendarVisualizationParser envInfo = do
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
  startTimeParser = option (dateTimeReader envInfo) (long "start-time" <> short 's' <> help "start time")
  endTimeParser = option (dateTimeReader envInfo) (long "end-time" <> short 'e' <> help "end time")

  timeRangeParser =
    TimeRange
      <$> startTimeParser
      <*> optional endTimeParser

parseVisualizeEventCliOption :: VisualizeEventOption -> IO CalendarSummaryOption
parseVisualizeEventCliOption cliOption = do
  defaultCacheFile <- getXdgDirectory XdgCache "task-utils/cache.json"
  defaultConfigFile <- getXdgDirectory XdgConfig "task-utils/config.json"
  timeZone <- getCurrentTimeZone
  time <- getCurrentTime
  let
    currentDay :: Day = utcToLocalTime timeZone time & localDay
  let
    configFile = fromMaybe defaultConfigFile cliOption.configPath
  config :: ConfigFromFile <- A.eitherDecodeFileStrict configFile <&> either error id
  let
    calendarDir = case cliOption.calendarDir of
      Just x -> x
      Nothing -> case config.calendarDir of
        Just x -> x
        Nothing -> error "calendarDir is not specified either in command line or in config file"
  timeRange <- do
    case cliOption.timeRange of
      Just (TimeRange startTime maybeEndTime) -> case maybeEndTime of
        Just endTime -> return (startTime, endTime)
        Nothing -> return (startTime, LocalTime{localDay = succ (localDay startTime), localTimeOfDay = startTime.localTimeOfDay})
      Nothing ->
        return
          ( LocalTime{localDay = currentDay, localTimeOfDay = TimeOfDay 0 0 0}
          , LocalTime{localDay = succ currentDay, localTimeOfDay = TimeOfDay 0 0 0}
          )
  return
    CalendarSummaryOption
      { calendarDir = calendarDir
      , timeRange = timeRange
      , outputPng = cliOption.outputPng
      , cacheJSONPath = fromMaybe defaultCacheFile $ cliOption.cacheJSONPath <|> config.cacheJSONPath
      , classifyConfig = config.classifyConfig
      }

totalParser :: EnvInfo -> Parser TotalOption
totalParser envInfo =
  let
    today = getToday envInfo
   in
    hsubparser
      ( command "visualize-task" (info (VisualizeTask <$> visParser) idm)
          <> command "get-task-closure" (info (GetTaskClosure <$> maybeFilterParser) idm)
          <> command "modify-task" (info (Mod <$> modParser) idm)
          <> command "add-event" (info (AddEvent <$> addEventParser envInfo) idm)
          <> command "list-event" (info (ListEvent <$> argument (dateReader envInfo) (metavar "DATE" <> value today)) idm)
          <> command "list-task" (info (ListTask <$> maybeFilterParser) idm)
          <> command "pending-task" (info (PendingTask <$> maybeFilterParser) idm)
          <> command "finish-task" (info (FinishTask <$> maybeFilterParser) idm)
          <> command "view-task" (info (ViewTask <$> maybeFilterParser) idm)
          <> command "add-task" (info (AddTask <$> many (argument str (metavar "TASK_INFO"))) idm)
          <> command "delete-task" (info (DeleteTask <$> filterParser) idm)
          <> command "visualize-event" (info (VisualizeEvent <$> calendarVisualizationParser envInfo) idm)
      )
