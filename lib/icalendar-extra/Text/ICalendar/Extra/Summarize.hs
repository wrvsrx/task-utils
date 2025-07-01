{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Text.ICalendar.Extra.Summarize (
  checkEvent,
  accountEvent,
) where

import Data.List (sort)
import Data.List.NonEmpty (nonEmpty)
import Data.Map qualified as M
import Data.Time (TimeZone, ZonedTime, diffUTCTime, utcToZonedTime)
import Text.ICalendar.Extra.Classify (EventType (..))
import Text.ICalendar.Extra.Types (Event (..), VisualizeEventWarning (..))

checkEndToEnd :: TimeZone -> [Event] -> [(ZonedTime, ZonedTime)]
checkEndToEnd _ [] = []
checkEndToEnd _ [_] = []
checkEndToEnd timeZone (x : y : xs) =
  if x.endTime /= y.startTime
    then (utcToZonedTime timeZone x.endTime, utcToZonedTime timeZone y.startTime) : checkEndToEnd timeZone (y : xs)
    else checkEndToEnd timeZone (y : xs)

checkEvent :: TimeZone -> [Event] -> Maybe VisualizeEventWarning
checkEvent tz evs =
  NotEndToEnd <$> nonEmpty (checkEndToEnd tz (sort evs))

accountEvent :: [(Event, EventType)] -> M.Map EventType Double
accountEvent =
  foldl
    (\mp (e, t) -> M.insertWith (+) t ((fromRational . toRational) (diffUTCTime e.endTime e.startTime)) mp)
    M.empty
