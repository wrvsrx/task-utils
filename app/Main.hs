{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cli
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Text.IO qualified as T
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
