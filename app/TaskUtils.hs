{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TaskUtils (
  listTask,
  listFromFilter,
  modTask,
  finishTask,
  addTask,
  dateToDay,
  Date (..),
)
where

import Data.Functor ((<&>))
import Data.List (intersperse)
import Data.Text qualified as T
import Data.Text.Lazy.IO qualified as TZIO
import Data.Time (
  Day (..),
  LocalTime (..),
  getCurrentTime,
  getCurrentTimeZone,
  utcToLocalTime,
 )
import System.Process (rawSystem)
import Taskwarrior.IO (getTasks)
import Taskwarrior.Status (
  Status (..),
 )
import Taskwarrior.Task (
  Task (..),
 )
import Text.DocLayout qualified as L

data Date = AbsoluteDate Day | RelativeDate Int

data TaskColumn = Id | Status | Description deriving (Show)

columnWidth :: TaskColumn -> Int
columnWidth = \case
  Id -> 5
  Description -> 30
  Status -> 11

columnPicker :: TaskColumn -> Task -> T.Text
columnPicker = \case
  Id -> T.pack . (\case Just x -> show x; Nothing -> "_") . (.id)
  Description -> description
  Status ->
    T.pack
      . ( \case
            Pending -> "Pending"
            Completed _ -> "Completed"
            Deleted _ -> "Deleted"
            Recurring _ _ -> "Recurring"
        )
      . (.status)

formatTasks :: [Task] -> L.Doc T.Text
formatTasks tasks =
  let
    cols = [Id, Description, Status]
    docHeader =
      L.hcat
        ( intersperse
            (L.vfill " ")
            ( map
                ( \col ->
                    L.lblock
                      (columnWidth col)
                      (L.underlined $ L.bold $ L.literal $ T.pack (show col))
                )
                cols
            )
        )
   in
    docHeader
      <> L.cr
      <> L.vcat (map (formatTaskLine cols) tasks)
      <> L.cr

formatTaskLine :: [TaskColumn] -> Task -> L.Doc T.Text
formatTaskLine cols task =
  L.hcat (intersperse (L.vfill " " :: L.Doc T.Text) (map (\col -> L.lblock (columnWidth col) (L.literal $ columnPicker col task)) cols))

listTask :: [Task] -> IO ()
listTask tasks = do
  if null tasks
    then putStrLn "No tasks found"
    else do
      let
        doc = formatTasks tasks
        t = L.renderANSI (Just 80) doc
      TZIO.putStr t

listFromFilter :: [T.Text] -> IO ()
listFromFilter filters = do
  tasks <- getTasks filters
  listTask tasks

modTask :: [T.Text] -> [T.Text] -> IO ()
modTask filters modfiers = do
  _ <- rawSystem "task" (map T.unpack filters <> ["mod"] <> map T.unpack modfiers)
  return ()

finishTask :: [T.Text] -> IO ()
finishTask filters = do
  _ <- rawSystem "task" (map T.unpack filters <> ["done"])
  return ()

addTask :: [T.Text] -> IO ()
addTask filters = do
  _ <- rawSystem "task" (["add"] <> map T.unpack filters)
  return ()

dateToDay :: Date -> IO Day
dateToDay date = do
  case date of
    AbsoluteDate day -> return day
    RelativeDate offset -> do
      tz <- getCurrentTimeZone
      today <- getCurrentTime <&> ((.localDay)) . utcToLocalTime tz
      let
        addTo :: Day -> Int -> Day
        addTo day n | n > 0 = addTo (succ day) (n - 1)
        addTo day n | n < 0 = addTo (pred day) (n + 1)
        addTo day _ = day
      return $ addTo today offset
