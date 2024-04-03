module Main where

import Cli
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Text.IO qualified as T
import Data.Time.LocalTime (getCurrentTimeZone)
import Options.Applicative (customExecParser, idm, info, prefs, showHelpOnEmpty)
import Task (RenderOption (..), taskJsonToDotImpure, taskJsonToDotPure, taskToClosure)

main :: IO ()
main = do
  totalOpt <- customExecParser (prefs showHelpOnEmpty) (info totalParser idm)
  case totalOpt of
    Right opt -> do
      let
        renderOpt = RenderOption{highlights = optHighlights opt, showDeleted = optDeleted opt, showOutside = optOutside opt}
      cnt <- BL.getContents
      tz <- getCurrentTimeZone
      if optImpure opt
        then cnt & taskJsonToDotImpure tz renderOpt >>= T.putStrLn
        else cnt & taskJsonToDotPure tz renderOpt & T.putStrLn
    Left _ -> do
      cnt <- BL.getContents
      t <- taskToClosure cnt
      T.putStrLn t
