{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task.Utils (
  listTask,
  TaskColumn (..),
  listFromFilter,
  modTask,
  finishTask,
  addTask,
  deleteTask,
  dateToDay,
  TaskDate (..),
  viewTask,
)
where

import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.Time (
  Day (..),
  LocalTime (..),
  getCurrentTime,
  getCurrentTimeZone,
  utcToLocalTime,
 )
import System.Process (rawSystem)
import Task.Renderer.Terminal (TaskColumn (..), listTask)
import Taskwarrior.IO (getTasks)
import Utils (TaskDate (..))

listFromFilter :: [T.Text] -> IO ()
listFromFilter filters = do
  tasks <- getTasks filters
  listTask [IdOrUUID, Description, Tags, Status, Urg] tasks

modTask :: [T.Text] -> [T.Text] -> IO ()
modTask filters modfiers = do
  _ <- rawSystem "task" (map T.unpack filters <> ["mod"] <> map T.unpack modfiers)
  return ()

finishTask :: [T.Text] -> IO ()
finishTask filters = do
  _ <- rawSystem "task" (map T.unpack filters <> ["done"])
  return ()

deleteTask :: [T.Text] -> IO ()
deleteTask filters = do
  _ <- rawSystem "task" (map T.unpack filters <> ["delete"])
  return ()

addTask :: [T.Text] -> IO ()
addTask filters = do
  _ <- rawSystem "task" (["add"] <> map T.unpack filters)
  return ()

dateToDay :: TaskDate -> IO Day
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

viewTask :: [T.Text] -> IO ()
viewTask filters = do
  _ <- rawSystem "task" (map T.unpack filters <> ["information"])
  return ()
