module Main where

import Cli
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as T
import Task

main :: IO ()
main = do
  opt <- parseVisOpt
  cnt <- BL.getContents
  T.putStrLn $ taskJsonToDot opt cnt
