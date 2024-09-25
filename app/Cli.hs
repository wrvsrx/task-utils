{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Cli (
  VisOption (..),
  ClosureOption (..),
  TotalOption (..),
  EventOption (..),
  ModOption (..),
  totalParser,
) where

import Data.Text qualified as T
import Data.Time (Day (..), defaultTimeLocale, parseTimeM)
import Options.Applicative

newtype ClosureOption = ClosureOption [T.Text]

data VisOption = VisOption
  { optHighlights :: [T.Text]
  , optDeleted :: Bool
  , optFilter :: [T.Text]
  }

data EventOption = EventOption
  { summary :: T.Text
  , start :: T.Text
  , end :: T.Text
  , task :: Maybe T.Text
  }

data TotalOption
  = Closure ClosureOption
  | Vis VisOption
  | Event EventOption
  | Mod ModOption
  | DateTag Day
  | Date (Maybe Day)

data ModOption = ModOption
  { filter :: [String]
  , modifier :: [String]
  }

parseHighlights :: String -> Either String [T.Text]
parseHighlights = Right . T.splitOn "," . T.pack

visParser :: Parser VisOption
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
    vis

closureParser :: Parser ClosureOption
closureParser = ClosureOption <$> many (argument str (metavar "FILTER"))

modParser :: Parser ModOption
modParser =
  ModOption
    <$> many (argument (maybeReader (\x -> if x /= "," then Just x else Nothing)) (metavar "FILTER"))
    <*> many (argument str (metavar "MODIFIER"))

eventParser :: Parser EventOption
eventParser =
  EventOption
    <$> argument str (metavar "SUMMARY")
    <*> argument str (metavar "START")
    <*> argument str (metavar "END")
    <*> optional (argument str (metavar "TASK"))

dateParser :: Parser Day
dateParser = argument reader (metavar "DATE")
 where
  reader = eitherReader $ \arg ->
    case parseTimeM True defaultTimeLocale "%Y%m%d" arg <|> parseTimeM True defaultTimeLocale "%Y-%m-%d" arg of
      Just x -> Right x
      Nothing -> Left "Failed to parse date. The supported formats are YYYYMMDD or YYYY-MM-DD."

totalParser :: Parser TotalOption
totalParser =
  hsubparser
    ( command "visualize" (info (Vis <$> visParser) idm)
        <> command "closure" (info (Closure <$> closureParser) idm)
        <> command "event" (info (Event <$> eventParser) idm)
        <> command "mod" (info (Mod <$> modParser) idm)
        <> command "date-tag" (info (DateTag <$> dateParser) idm)
        <> command "date" (info (Date <$> optional dateParser) idm)
    )
