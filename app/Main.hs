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
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getCurrentTimeZone)
import Options.Applicative (customExecParser, idm, info, prefs, showHelpOnEmpty)
import System.Process (rawSystem)
import Task (
  RenderOption (..),
  getClosureImpure,
  taskDeserialize,
  tasksToDotImpure,
 )
import TaskUtils (
  listFromFilter,
  listTask,
  modTask,
 )
import Taskwarrior.IO (getTasks)
import Taskwarrior.Task (Task (..))

main :: IO ()
main = do
  totalOpt <- customExecParser (prefs showHelpOnEmpty) (info totalParser idm)
  case totalOpt of
    Vis opt -> do
      let
        renderOpt = RenderOption{highlights = opt.highlights, showDeleted = opt.deleted}
      tasks <-
        if opt.filters == ["-"]
          then taskDeserialize <$> BL.getContents
          else getTasks opt.filters
      tz <- getCurrentTimeZone
      tasks & tasksToDotImpure tz renderOpt >>= T.putStrLn
    Closure (ClosureOption filters) -> do
      tasks <- getTasks filters
      taskClosure <- getClosureImpure tasks
      listTask taskClosure
    Event (EventOption summary start' end task) -> do
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
          ["new", T.unpack start', T.unpack end, T.unpack summary] <> case task' of
            Just t -> ["::", BLU.toString $ A.encode (A.object ["task" .= t.uuid])]
            Nothing -> []
      return ()
    Mod (ModOption filters modifiers) -> modTask filters modifiers
    DateTag day -> modTask ["entry:" <> show day] [formatTime defaultTimeLocale "+d%Y%m%d" day]
    Date maybeDay -> listFromFilter ["entry:" <> maybe "today" show maybeDay]
    Search filters -> listFromFilter filters
