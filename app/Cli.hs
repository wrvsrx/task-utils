{-# LANGUAGE OverloadedStrings #-}

module Cli (
  parseVisOpt,
  parseVisOptPure,
  VisOption (..),
) where

import Data.Text qualified as T
import Options.Applicative

data VisOption = VisOption
  { optHighlights :: [T.Text]
  , optImpure :: Bool
  , optOutside :: Bool
  , optDeleted :: Bool
  }

parseHighlights :: String -> Either String [T.Text]
parseHighlights = Right . T.splitOn "," . T.pack

optParser :: Parser VisOption
optParser =
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

visOptionInfo :: ParserInfo VisOption
visOptionInfo =
  info
    (optParser <**> helper)
    ( fullDesc
        <> progDesc "taskwarrior-to-dot: convert taskwarrior json to dot"
    )

parseVisOptPure :: [String] -> ParserResult VisOption
parseVisOptPure = execParserPure (prefs mempty) visOptionInfo

parseVisOpt :: IO VisOption
parseVisOpt = execParser visOptionInfo
