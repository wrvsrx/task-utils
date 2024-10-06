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
  deleteTask,
  dateToDay,
  Date (..),
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
import TaskUtils.Renderer.Terminal (listTask)
import Taskwarrior.IO (getTasks)

data Date = AbsoluteDate Day | RelativeDate Int

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

deleteTask :: [T.Text] -> IO ()
deleteTask filters = do
  _ <- rawSystem "task" (map T.unpack filters <> ["delete"])
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
