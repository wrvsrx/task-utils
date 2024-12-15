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
import Event (visualizeEvent)
import Options.Applicative (customExecParser, idm, info, prefs, showHelpOnEmpty)
import System.Process (rawSystem)
import Task (
  RenderOption (..),
  TaskColumn (..),
  addTask,
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

main :: IO ()
main = do
  envInfo <- getEnvInfo
  totalOpt <- customExecParser (prefs showHelpOnEmpty) (info (totalParser envInfo) idm)
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
    AddEvent (AddEventOption summary start' end task) -> do
      task' <- do
        case task of
          Just t -> do
            ts <- getTasks [t]
            case ts of
              [t'] -> return (Just t')
              [] -> fail "Task not found"
              _ -> fail "Multiple tasks found"
          Nothing -> return Nothing
      _ <-
        rawSystem "khal" $
          let
            f = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M"
           in
            ["new", f start', f end, T.unpack summary] <> case task' of
              Just t -> ["::", BLU.toString $ A.encode (A.object ["task" .= t.uuid])]
              Nothing -> []
      return ()
    Mod (ModOption filter' modifiers) -> modTask (getFilters (Just filter')) modifiers
    ListTask filter' -> listFromFilter (getFilters filter')
    PendingTask filter' -> do
      let
        filters = keepPendingFilters (getFilters filter')
      tasks <- getTasks filters
      listTask [IdOrUUID, Description, Tags, Urg] tasks
    ListEvent day -> do
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
