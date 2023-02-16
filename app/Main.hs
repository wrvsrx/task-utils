module Main where

import Cli qualified
import Data.ByteString.Lazy qualified as BL
import Data.Text.IO qualified as T
import Task (RenderOption (RenderOption, highlights, showDeleted, showOutside), taskJsonToDotImpure, taskJsonToDotPure)

main :: IO ()
main = do
  opt <- Cli.parseVisOpt
  cnt <- BL.getContents
  let
    renderOpt = RenderOption{highlights = Cli.highlights opt, showDeleted = Cli.deleted opt, showOutside = Cli.outside opt}
  if Cli.impure opt
    then taskJsonToDotImpure renderOpt cnt >>= T.putStrLn
    else T.putStrLn $ taskJsonToDotPure renderOpt cnt
