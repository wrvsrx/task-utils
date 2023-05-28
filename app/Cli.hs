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
      )
    <*> flag False True (long "impure" <> short 'i')
    <*> flag False True (long "outside" <> short 'o')
    <*> flag False True (long "deleted" <> short 'd')

visOptionInfo :: ParserInfo VisOption
visOptionInfo =
  info
    (optParser <**> helper)
    ( fullDesc
        <> progDesc "taskwarrior_to_dot: convert taskwarrior json to dot"
    )

parseVisOptPure :: [String] -> ParserResult VisOption
parseVisOptPure = execParserPure (prefs mempty) visOptionInfo

parseVisOpt :: IO VisOption
parseVisOpt = execParser visOptionInfo
