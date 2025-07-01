{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Text.ICalendar.Extra.Classify (
  EventType (..),
  classifyEvent,
  ClassifyConfig (..),
  ClassifyError (..),
) where

import Data.Aeson qualified as A
import Data.List.Split (splitOn)
import Data.Map qualified as M
import GHC.Generics (Generic)
import Text.ICalendar.Extra.Types (Event (..), EventType (..))

data ClassifyConfig = ClassifyConfig
  { eventTypes :: [EventType]
  , eventToType :: M.Map String EventType
  , eventAbbrToType :: M.Map String EventType
  }
  deriving (Generic, Show)

data ClassifyError = ClassifyError
  { errorMessage :: String
  , event :: Event
  }
  deriving (Show)

instance A.FromJSON ClassifyConfig
instance A.ToJSON ClassifyConfig

classifyEvent :: ClassifyConfig -> Event -> Either ClassifyError (Maybe EventType)
classifyEvent config event =
  -- summary pattern
  -- '<category>: <description>'
  -- '<summary>'
  let
    summary = event.summary
    splitResult = splitOn ":" summary
    et = case splitResult of
      [eventTypeAbbr, description] -> do
        case description of
          ' ' : _ -> case M.lookup eventTypeAbbr config.eventAbbrToType of
            Just x -> Right (Just x)
            Nothing -> Left $ ClassifyError ("can't find such type: " <> eventTypeAbbr) event
          _ -> Left $ ClassifyError "no space ahead of name" event
      [summary_] ->
        let
          maybeEventType = M.lookup summary_ config.eventToType
         in
          case maybeEventType of
            Just et_ -> Right (Just et_)
            Nothing -> Right Nothing
      [] -> Left (ClassifyError "impossible case" event)
      _ -> Left (ClassifyError "too much `:`" event)
   in
    et
