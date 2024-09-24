{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TaskUtils (
  listTask,
  dateFilter,
)
where

import Data.Text qualified as T
import Data.Time (Day)
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

dateFilter :: Day -> [T.Text]
dateFilter day = ["entry.before:" <> T.pack (show (succ day)), "entry.after:" <> T.pack (show day)]
