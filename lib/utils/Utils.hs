module Utils (
  TaskDate (..),
  TimeRange (..),
  dateToDay,
) where

import Data.Functor ((<&>))
import Data.Time (
  Day (..),
  LocalTime (..),
  getCurrentTime,
  getCurrentTimeZone,
  utcToLocalTime,
 )

data TaskDate = AbsoluteDate Day | RelativeDate Int deriving (Show)
data TimeRange
  = TimeRangeDay TaskDate
  | TimeRangeRange LocalTime (Maybe LocalTime)
  deriving (Show)

dateToDay :: TaskDate -> IO Day
dateToDay date = do
  case date of
    AbsoluteDate day -> return day
    RelativeDate offset -> do
      tz <- getCurrentTimeZone
      today <- getCurrentTime <&> localDay . utcToLocalTime tz
      let
        addTo :: Day -> Int -> Day
        addTo day n | n > 0 = addTo (succ day) (n - 1)
        addTo day n | n < 0 = addTo (pred day) (n + 1)
        addTo day _ = day
      return $ addTo today offset
