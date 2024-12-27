{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Text.ICalendar.Extra.Types (
  Event (..),
  Todo (..),
) where

import Control.DeepSeq (NFData)
import Data.Aeson qualified as A
import Data.Time (
  UTCTime,
 )
import GHC.Generics (Generic)

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
