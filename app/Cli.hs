{-# LANGUAGE OverloadedStrings #-}

module Cli (
  VisOption (..),
  ClosureOption (..),
  TotalOption (..),
  totalParser,
) where

import Data.Text qualified as T
import Options.Applicative

newtype ClosureOption = ClosureOption [T.Text]

data VisOption = VisOption
  { optHighlights :: [T.Text]
  , optDeleted :: Bool
  , optFilter :: [T.Text]
  }

data TotalOption
  = Closure ClosureOption
  | Vis VisOption
  | Today

parseHighlights :: String -> Either String [T.Text]
parseHighlights = Right . T.splitOn "," . T.pack

visParser :: Parser TotalOption
visParser =
  let
    vis =
      VisOption
        <$> option
          (eitherReader parseHighlights)
          ( long "highlights"
              <> short 'h'
              <> value []
              <> help "Tags to be highlighted."
          )
        <*> flag False True (long "deleted" <> short 'd' <> help "Show deleted tasks.")
        <*> many (argument str (metavar "FILTER"))
   in
    Vis <$> vis

closureParser :: Parser TotalOption
closureParser = Closure . ClosureOption <$> many (argument str (metavar "FILTER"))

todayClosure :: Parser TotalOption
todayClosure = pure Today

totalParser :: Parser TotalOption
totalParser =
  hsubparser
    ( command "visualize" (info visParser idm)
        <> command "closure" (info closureParser idm)
        <> command "today" (info todayClosure idm))
