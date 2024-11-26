{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Event.Summarize (
  checkEvent,
  accountEvent,
  CheckError (..),
) where

import Data.List (sort)
import Data.Map qualified as M
import Data.Time (TimeZone, ZonedTime (..), diffUTCTime, utcToZonedTime)
import Event.Classify (EventType (..))
import Event.Event (Event (..))

data ZonedEvent = ZonedEvent
  { summary :: String
  , start :: ZonedTime
  , end :: ZonedTime
  }
  deriving (Show)

eventToZonedEvent :: TimeZone -> Event -> ZonedEvent
eventToZonedEvent tz (Event su s e) = ZonedEvent su (utcToZonedTime tz s) (utcToZonedTime tz e)

data CheckError = NotEndToEnd [(ZonedEvent, ZonedEvent)] deriving (Show)

checkEndToEnd :: TimeZone -> [Event] -> [(ZonedEvent, ZonedEvent)]
checkEndToEnd _ [] = []
checkEndToEnd _ [_] = []
checkEndToEnd timeZone (x : y : xs) =
  if x.endTime /= y.startTime
    then (eventToZonedEvent timeZone x, eventToZonedEvent timeZone y) : checkEndToEnd timeZone (y : xs)
    else checkEndToEnd timeZone (y : xs)

checkEvent :: TimeZone -> [Event] -> Maybe CheckError
checkEvent tz evs =
  let
    res = checkEndToEnd tz (sort evs)
   in
    if null res then Nothing else Just (NotEndToEnd res)

accountEvent :: [(Event, EventType)] -> M.Map EventType Double
accountEvent =
  foldl
    (\mp (e, t) -> M.insertWith (+) t ((fromRational . toRational) (diffUTCTime e.endTime e.startTime)) mp)
    M.empty
