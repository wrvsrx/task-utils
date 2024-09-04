{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cli
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (LocalTime (localDay), getCurrentTime, getTimeZone, showGregorian, utcToLocalTime)
import Data.Time.LocalTime (getCurrentTimeZone)
import Options.Applicative (customExecParser, idm, info, prefs, showHelpOnEmpty)
import Task (
  RenderOption (..),
  getClosureImpure,
  taskDeserialize,
  tasksToDotImpure,
 )
import Taskwarrior.IO (getTasks)
import Text.Pretty.Simple (pPrint)

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
      pPrint taskClosure
    Today -> do
      tz <- getCurrentTimeZone
      today <- getCurrentTime <&> utcToLocalTime tz <&> (.localDay)
      let
        todayS = showGregorian today
        filters =
          [ "entry.after:" <> showGregorian today <> "T00:00:00"
          , "entry.before:" <> showGregorian (succ today) <> "T00:00:00"
          ]
      tasks <- getTasks (map T.pack filters)
      pPrint tasks
