module Utils (
  TaskDate (..),
  TimeRange (..),
) where

import Data.Time (Day (..), LocalTime (..))

data TaskDate = AbsoluteDate Day | RelativeDate Int deriving (Show)
data TimeRange
  = TimeRangeDay TaskDate
  | TimeRangeRange LocalTime (Maybe LocalTime)
  deriving (Show)
