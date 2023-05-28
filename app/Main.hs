module Main where

import Cli
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Text.IO qualified as T
import Task (RenderOption (..), taskJsonToDotImpure, taskJsonToDotPure)

main :: IO ()
main = do
  opt <- parseVisOpt
  let
    renderOpt = RenderOption{highlights = optHighlights opt, showDeleted = optDeleted opt, showOutside = optOutside opt}
  cnt <- BL.getContents
  if optImpure opt
    then cnt & taskJsonToDotImpure renderOpt >>= T.putStrLn
    else cnt & taskJsonToDotPure renderOpt & T.putStrLn
