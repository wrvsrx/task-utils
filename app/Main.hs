{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Cli
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (
  LocalTime (localDay),
  getCurrentTime,
  showGregorian,
  utcToLocalTime,
 )
import Data.Time.LocalTime (getCurrentTimeZone)
import Options.Applicative (customExecParser, idm, info, prefs, showHelpOnEmpty)
import System.Process (rawSystem)
import Task (
  RenderOption (..),
  getClosureImpure,
  taskDeserialize,
  tasksToDotImpure,
 )
import Taskwarrior.IO (getTasks)
import Taskwarrior.Task (Task (..))

listTask :: [Task] -> IO ()
listTask tasks = do
  let
    uuids = map (show . (.uuid)) tasks
  _ <- rawSystem "task" (uuids <> ["list"])
  return ()

main :: IO ()
main = do
  totalOpt <- customExecParser (prefs showHelpOnEmpty) (info totalParser idm)
  case totalOpt of
    Vis opt -> do
      let
        renderOpt = RenderOption{highlights = optHighlights opt, showDeleted = optDeleted opt}
      tasks <-
        if optFilter opt == ["-"]
          then taskDeserialize <$> BL.getContents
          else getTasks (optFilter opt)
      tz <- getCurrentTimeZone
      tasks & tasksToDotImpure tz renderOpt >>= T.putStrLn
    Closure (ClosureOption filters) -> do
      tasks <- getTasks filters
      taskClosure <- getClosureImpure tasks
      listTask taskClosure
    Today -> do
      tz <- getCurrentTimeZone
      today <- getCurrentTime <&> utcToLocalTime tz <&> (.localDay)
      let
        filters =
          [ "entry.after:" <> showGregorian today <> "T00:00:00"
          , "entry.before:" <> showGregorian (succ today) <> "T00:00:00"
          ]
      tasks <- getTasks (map T.pack filters)
      listTask tasks
