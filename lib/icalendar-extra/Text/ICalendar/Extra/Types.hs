{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Text.ICalendar.Extra.Types (
  Event (..),
  EventType (..),
  Todo (..),
  VisualizeEventWarning (..),
) where

import Control.DeepSeq (NFData)
import Data.Aeson ((.:))
import Data.Aeson qualified as A
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import Data.Time (
  UTCTime,
  ZonedTime (..),
 )
import GHC.Generics (Generic)

newtype EventType = EventType {typeName :: String} deriving (Generic, Eq, Ord, Show)

instance A.FromJSON EventType where
  parseJSON a = case a of
    A.Object o -> EventType <$> (o .: "typeName")
    A.String s -> return (EventType (T.unpack s))
    _ -> fail "fail to parse"
instance A.ToJSON EventType

-- startTime should large than endTime
data Event = Event
  { summary :: String
  , startTime :: UTCTime
  , endTime :: UTCTime
  }
  deriving (Show, Generic, Eq)

newtype Todo = Todo {unwrap :: ()} deriving (Show, Generic, Eq)

instance A.FromJSON Event
instance A.ToJSON Event

instance A.FromJSON Todo
instance A.ToJSON Todo

instance NFData Event
instance Ord Event where
  compare l r =
    if l.startTime == r.startTime
      then
        if l.endTime == r.endTime
          then compare l.summary r.summary
          else compare l.endTime r.endTime
      else compare l.startTime r.startTime

data VisualizeEventWarning
  = NotEndToEnd (NonEmpty (ZonedTime, ZonedTime))
  | FileTimeEarlierThanCacheTime FilePath
  | UnknownEvents (NonEmpty (Event, Maybe EventType))
  deriving (Show, Generic)
instance A.ToJSON VisualizeEventWarning
