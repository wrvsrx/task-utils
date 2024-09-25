{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TaskUtils (
  listTask,
  listFromFilter,
  modTask,
)
where

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

listFromFilter :: [String] -> IO ()
listFromFilter filters = do
  _ <- rawSystem "task" (filters <> ["all"])
  return ()

modTask :: [String] -> [String] -> IO ()
modTask filters modfiers = do
  _ <- rawSystem "task" (filters <> ["mod"] <> modfiers)
  return ()
