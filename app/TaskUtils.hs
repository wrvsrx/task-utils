{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TaskUtils (
  listTask,
  listFromFilter,
  modTask,
  finishTask,
  getToday,
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
import Taskwarrior.Task (Task (..))

listTask :: [Task] -> IO ()
listTask tasks = do
  let
    uuids = map (show . (.uuid)) tasks
  if null tasks
    then putStrLn "No tasks found"
    else do
      _ <- rawSystem "task" (uuids <> ["all"])
      return ()

listFromFilter :: [T.Text] -> IO ()
listFromFilter filters = do
  _ <- rawSystem "task" (map T.unpack filters <> ["all"])
  return ()

modTask :: [T.Text] -> [T.Text] -> IO ()
modTask filters modfiers = do
  _ <- rawSystem "task" (map T.unpack filters <> ["mod"] <> map T.unpack modfiers)
  return ()

finishTask :: [T.Text] -> IO ()
finishTask filters = do
  _ <- rawSystem "task" (map T.unpack filters <> ["done"])
  return ()

getToday :: IO Day
getToday = do
  tz <- getCurrentTimeZone
  getCurrentTime <&> ((.localDay)) . utcToLocalTime tz
