{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Cli
import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.UTF8 qualified as BLU
import Data.Function ((&))
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (
  defaultTimeLocale,
  formatTime,
 )
import Data.Time.LocalTime (getCurrentTimeZone)
import Options.Applicative (customExecParser, idm, info, prefs, showHelpOnEmpty)
import System.Process (rawSystem)
import Task (
  RenderOption (..),
  TaskColumn (..),
  addTask,
  dateToDay,
  deleteTask,
  finishTask,
  getClosureImpure,
  listFromFilter,
  listTask,
  modTask,
  parseFilter,
  taskDeserialize,
  tasksToDotImpure,
  viewTask,
 )
import Taskwarrior.IO (getTasks)
import Taskwarrior.Task (Task (..))
import Text.ICalendar.Extra.VisualizeEvent (visualizeEvent)

main :: IO ()
main = do
  totalOpt <- customExecParser (prefs showHelpOnEmpty) (info totalParser idm)
  case totalOpt of
    VisualizeTask opt -> do
      let
        renderOpt = RenderOption{highlights = opt.highlights, showDeleted = opt.deleted}
      tasks <-
        if opt.filter == Just "-"
          then taskDeserialize <$> BL.getContents
          else getTasks (getFilters opt.filter)
      tz <- getCurrentTimeZone
      tasks & tasksToDotImpure tz renderOpt >>= T.putStrLn
    GetTaskClosure filter' -> do
      tasks <- getTasks (getFilters filter')
      taskClosure <- getClosureImpure tasks
      listTask [IdOrUUID, Description, Tags, Status, Urg] taskClosure
    AddEvent (EventOption summary start' end filter') -> do
      maybeEventDescription <- case filter' of
        Nothing -> return Nothing
        Just filter'' -> do
          tasks <- getTasks (getFilters (Just filter''))
          case tasks of
            [task] -> return $ Just $ BLU.toString $ A.encode (A.object ["task" .= task.uuid])
            [] -> error "No task found"
            _ -> error "Multiple tasks found"
      _ <-
        rawSystem "khal" $
          ["new", T.unpack start', T.unpack end, T.unpack summary] <> case maybeEventDescription of
            Just description' -> ["::", description']
            Nothing -> []
      return ()
    Mod (ModOption filter' modifiers) -> modTask (getFilters (Just filter')) modifiers
    ListTask filter' -> listFromFilter (getFilters filter')
    PendingTask filter' -> do
      let
        filters = keepPendingFilters (getFilters filter')
      tasks <- getTasks filters
      listTask [IdOrUUID, Description, Tags, Urg] tasks
    ListEvent date -> do
      day <- dateToDay date
      _ <- rawSystem "khal" ["list", formatTime defaultTimeLocale "%Y-%m-%d" day]
      return ()
    FinishTask filter' -> finishTask (keepPendingFilters (getFilters filter'))
    ViewTask filter' -> do
      viewTask (getFilters filter')
    AddTask taskInfos -> addTask taskInfos
    DeleteTask filter' -> deleteTask (getFilters (Just filter'))
    VisualizeEvent opt -> do
      opt' <- parseVisualizeEventCliOption opt
      visualizeEvent opt'
 where
  getFilters x =
    case x of
      Nothing -> []
      Just x' ->
        let
          parseResult = parseFilter x'
         in
          either (error . show) Prelude.id parseResult
  keepPendingFilters :: [T.Text] -> [T.Text]
  keepPendingFilters x = ["("] <> x <> [")", "status:pending"]
