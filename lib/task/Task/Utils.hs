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
  viewTask,
)
where

import Data.Text qualified as T
import System.Process (rawSystem)
import Task.Renderer.Terminal (TaskColumn (..), listTask)
import Taskwarrior.IO (getTasks)

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

viewTask :: [T.Text] -> IO ()
viewTask filters = do
  _ <- rawSystem "task" (map T.unpack filters <> ["information"])
  return ()
