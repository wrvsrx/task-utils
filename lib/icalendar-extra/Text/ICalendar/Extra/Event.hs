{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Text.ICalendar.Extra.Event (
  visualizeEvent,
  CalendarSummaryOption (..),
  ConfigFromFile (..),
) where

import Control.Monad (unless)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Data.Aeson qualified as A
import Data.Bifunctor (Bifunctor (second), bimap)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text.Lazy qualified as T
import Data.Time (
  LocalTime (..),
  TimeZone,
  getCurrentTimeZone,
  localTimeToUTC,
 )
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Text.ICalendar.Extra.Classify (ClassifyConfig (..), EventType (..), classifyEvent)
import Text.ICalendar.Extra.Draw (toPng)
import Text.ICalendar.Extra.ParseVDirSyncer (
  CalendarContent (..),
  filterAccordingToTime,
  parseCalendarsUsingCache,
 )
import Text.ICalendar.Extra.Summarize (
  accountEvent,
  checkEvent,
  formatCheckError,
 )
import Text.Pretty.Simple (pShow)
import Text.Show.Unicode (uprint, ushow)

data ConfigFromFile = ConfigFromFile
  { calendarDir :: Maybe FilePath
  , cacheJSONPath :: Maybe FilePath
  , classifyConfig :: ClassifyConfig
  }
  deriving (Show, Generic)

instance A.FromJSON ConfigFromFile
instance A.ToJSON ConfigFromFile

data CalendarSummaryOption = CalendarSummaryOption
  { calendarDir :: FilePath
  , timeRange :: (LocalTime, LocalTime)
  , outputPng :: FilePath
  , cacheJSONPath :: FilePath
  , classifyConfig :: ClassifyConfig
  }

mainFunc :: TimeZone -> CalendarSummaryOption -> ExceptT String (WriterT [String] IO) ()
mainFunc timeZone options = do
  let
    cacheDir = takeDirectory options.cacheJSONPath
  l2 $ createDirectoryIfMissing True cacheDir
  calendarContents <- parseCalendarsUsingCache options.cacheJSONPath options.calendarDir
  let
    events =
      mapMaybe
        ( \case
            EventContent a -> Just a
            TodoContent _ -> Nothing
        )
        calendarContents
    eventsInRange = mapMaybe (filterAccordingToTime (bimap f f options.timeRange)) events
     where
      f = localTimeToUTC timeZone
    classfiedEvent = zip eventsInRange (map (either (error . T.unpack . pShow) id . classifyEvent options.classifyConfig) eventsInRange)
    unknownEvents = filter (\(_, t) -> null t) classfiedEvent
    checkEventRes = checkEvent timeZone eventsInRange
    statistics = accountEvent (map (second (fromMaybe (EventType "unknown"))) classfiedEvent)
    eventsWithTimeCost = M.toList statistics
  unless (null unknownEvents) $
    lift $
      tell ["UnknownEvents: " <> ushow unknownEvents]
  case checkEventRes of
    Just err -> lift $ tell $ formatCheckError err
    Nothing -> pure ()
  let
    totalTime = foldl (\a (_, b) -> a + b) 0.0 eventsWithTimeCost
  lift $ tell ["totalTime: " <> show totalTime]
  l2 $ toPng options.outputPng eventsWithTimeCost
  l2 $ mapM_ (\(EventType a, t) -> uprint (a, t / 3600.0)) eventsWithTimeCost
 where
  l2 = lift . lift

visualizeEvent :: CalendarSummaryOption -> IO ()
visualizeEvent options = do
  timeZone <- getCurrentTimeZone
  (runResult, warnings) <- runWriterT $ runExceptT $ mainFunc timeZone options
  putStrLn "warnings: "
  mapM_ (\x -> putStrLn $ "\t" <> x) warnings
  case runResult of
    Left err -> putStrLn $ "error: " <> err
    Right () -> return ()
