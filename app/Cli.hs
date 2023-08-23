{-# LANGUAGE OverloadedStrings #-}

module Cli (
  VisOption (..),
  ClosureOption (..),
  TotalOption,
  totalParser,
) where

import Data.Text qualified as T
import Options.Applicative

data ClosureOption = ClosureOption

data VisOption = VisOption
  { optHighlights :: [T.Text]
  , optImpure :: Bool
  , optOutside :: Bool
  , optDeleted :: Bool
  }

type TotalOption = Either ClosureOption VisOption

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
        <*> flag False True (long "impure" <> short 'i' <> help "Enable impure mode. In impure mode, dependency closure will be visualized.")
        <*> flag False True (long "outside" <> short 'o' <> help "Show tasks not appearing in input json.")
        <*> flag False True (long "deleted" <> short 'd' <> help "Show deleted tasks.")
   in
    Right <$> vis

closureParser :: Parser TotalOption
closureParser = pure (Left ClosureOption)

totalParser :: Parser TotalOption
totalParser =
  hsubparser
    ( command "visualize" (info visParser idm)
        <> command "closure" (info closureParser idm)
    )
