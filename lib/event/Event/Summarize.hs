{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Event.Summarize (
  checkEvent,
  accountEvent,
  formatCheckError,
  CheckError (..),
) where

import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty, toList)
import Data.Map qualified as M
import Data.Time (TimeZone, ZonedTime (..), diffUTCTime, utcToZonedTime)
import Event.Classify (EventType (..))
import Event.Event (Event (..))
import Text.Printf (printf)

data ZonedEvent = ZonedEvent
  { summary :: String
  , start :: ZonedTime
  , end :: ZonedTime
  }
  deriving (Show)

eventToZonedEvent :: TimeZone -> Event -> ZonedEvent
eventToZonedEvent tz (Event su s e) = ZonedEvent su (utcToZonedTime tz s) (utcToZonedTime tz e)

newtype CheckError = NotEndToEnd (NonEmpty (ZonedEvent, ZonedEvent)) deriving (Show)

formatCheckError :: CheckError -> [String]
formatCheckError (NotEndToEnd xs) =
  "Not End to End:" : concatMap formatPair (toList xs)
 where
  formatPair :: (ZonedEvent, ZonedEvent) -> [String]
  formatPair (x, y) =
    [ printf "-    %s (\"%s\"'s end)" (show x.end) x.summary
    , printf "  != %s (\"%s\"'s start)" (show y.start) y.summary
    ]

checkEndToEnd :: TimeZone -> [Event] -> [(ZonedEvent, ZonedEvent)]
checkEndToEnd _ [] = []
checkEndToEnd _ [_] = []
checkEndToEnd timeZone (x : y : xs) =
  if x.endTime /= y.startTime
    then (eventToZonedEvent timeZone x, eventToZonedEvent timeZone y) : checkEndToEnd timeZone (y : xs)
    else checkEndToEnd timeZone (y : xs)

checkEvent :: TimeZone -> [Event] -> Maybe CheckError
checkEvent tz evs =
  NotEndToEnd <$> nonEmpty (checkEndToEnd tz (sort evs))

accountEvent :: [(Event, EventType)] -> M.Map EventType Double
accountEvent =
  foldl
    (\mp (e, t) -> M.insertWith (+) t ((fromRational . toRational) (diffUTCTime e.endTime e.startTime)) mp)
    M.empty
