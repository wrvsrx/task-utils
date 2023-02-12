module Main where

import Cli qualified
import Data.ByteString.Lazy qualified as BL
import Data.Text.IO qualified as T
import Task (taskJsonToDotImpure, taskJsonToDotPure)

main :: IO ()
main = do
  opt <- Cli.parseVisOpt
  cnt <- BL.getContents
  if Cli.impure opt
    then taskJsonToDotImpure (Cli.highlights opt) cnt >>= T.putStrLn
    else T.putStrLn $ taskJsonToDotPure (Cli.highlights opt) cnt
