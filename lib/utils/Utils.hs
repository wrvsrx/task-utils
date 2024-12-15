module Utils (
  TimeRange (..),
) where

import Data.Time (
  LocalTime (..),
 )

data TimeRange = TimeRange LocalTime (Maybe LocalTime) deriving (Show)
