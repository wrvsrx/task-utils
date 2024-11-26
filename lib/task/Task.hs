{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Task (
  tasksToDotImpure,
  getClosureImpure,
  RenderOption (..),
  taskDeserialize,
  TaskDate (..),
  parseFilter,
  TaskColumn (..),
  addTask,
  dateToDay,
  deleteTask,
  finishTask,
  listFromFilter,
  listTask,
  modTask,
  viewTask,
) where

import Control.Arrow ((>>>))
import Data.Aeson qualified as A
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Graph.Inductive (Gr, Node, mkGraph)
import Data.GraphViz qualified as GV
import Data.GraphViz.Attributes.HTML as GH
import Data.List (intersect)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set ((\\))
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time.LocalTime (TimeZone, localDay, utcToLocalTime)
import Data.Tuple (swap)
import Data.UUID (UUID)
import Data.UUID qualified as UU
import Task.FilterParser (parseFilter)
import Task.Utils (
  TaskColumn (..),
  TaskDate (..),
  addTask,
  dateToDay,
  deleteTask,
  finishTask,
  listFromFilter,
  listTask,
  modTask,
  viewTask,
 )
import Taskwarrior.IO qualified as Ta
import Taskwarrior.Status qualified as Ta
import Taskwarrior.Task (Task)
import Taskwarrior.Task qualified as Ta

taskDeserialize :: BL.ByteString -> [Task]
taskDeserialize = fromJust . (A.decode :: BL.ByteString -> Maybe [Task])

getDepsNodeOfATask :: M.Map UUID Node -> Task -> [Node]
getDepsNodeOfATask m = Ta.depends >>> S.toList >>> map (fromJust . (`M.lookup` m))

getEdgesOfTaskInClosure :: M.Map UUID Int -> (Task, Node) -> [(Node, Node)]
getEdgesOfTaskInClosure m (t, n) = getDepsNodeOfATask m t & map (n,)

-- closed input -> closed output
-- open input -> ? output
purgeDeleted :: [Task] -> [Task]
purgeDeleted ts =
  let
    deletedUUIDSet =
      ts
        & filter (\t -> case Ta.status t of Ta.Deleted _ -> True; _ -> False)
        & map Ta.uuid
        & S.fromList
   in
    ts
      & filter (\t -> case Ta.status t of Ta.Deleted _ -> False; _ -> True)
      & map (\t -> t{Ta.depends = Ta.depends t \\ deletedUUIDSet})

getClosureImpure :: [Task] -> IO [Task]
getClosureImpure ts =
  let
    tasksUUID = map Ta.uuid ts
    tasksUUIDSet = S.fromList tasksUUID
    depsUUIDSet = mconcat (map Ta.depends ts)
    outsideDepsUUIDSet = depsUUIDSet \\ tasksUUIDSet
   in
    if null outsideDepsUUIDSet
      then return ts
      else do
        outsideTasks <- Ta.getTasks (map UU.toText (S.toList outsideDepsUUIDSet))
        getClosureImpure (ts <> outsideTasks)

-- Must input a closure
tasksClosureToGraph :: [Task] -> Gr Task ()
tasksClosureToGraph ts =
  let
    tasksWithNode = ts `zip` [(1 :: Node) ..]
    mapFromUUIDToNode = tasksWithNode & map (first Ta.uuid) & M.fromList
    nodes = tasksWithNode & map swap
    edges =
      tasksWithNode
        & concatMap (getEdgesOfTaskInClosure mapFromUUIDToNode)
        & map (\(x, y) -> (x, y, ()))
   in
    mkGraph nodes edges

taskGraphVis :: TimeZone -> [Ta.Tag] -> Gr Task () -> GV.DotGraph Node
taskGraphVis tz hls =
  GV.graphToDot
    ( GV.nonClusteredParams
        { GV.fmtNode = \(_, t) ->
            [ GV.toLabel $
                let
                  des = TL.fromStrict $ Ta.description t
                  startTime = case Ta.wait t of
                    Just w -> w
                    Nothing -> Ta.entry t
                  dueTime = Ta.due t
                  timeToLocalDate = utcToLocalTime tz >>> localDay
                  startEndStr = TL.pack (show (timeToLocalDate startTime) <> " -- " <> maybe "" (show . timeToLocalDate) dueTime)
                  uuid = TL.fromStrict $ maybe "" ((<> ",") . T.pack . show) (Ta.id t) <> T.take 8 (UU.toText (Ta.uuid t))
                  toUnderline x = GH.Format GH.Underline [GH.Str x]
                  textToTextItem =
                    let
                      st = case Ta.status t of Ta.Deleted _ -> True; _ -> False
                      tag = "giveup" `S.member` Ta.tags t
                     in
                      if st || tag then toUnderline else GH.Str
                 in
                  GH.Text
                    [ textToTextItem des
                    , GH.Newline []
                    , textToTextItem startEndStr
                    , GH.Newline []
                    , textToTextItem uuid
                    ]
            ]
              <> case Ta.status t of
                Ta.Completed _ -> [GV.fontColor GV.Gray, GV.color GV.Gray]
                Ta.Pending -> []
                Ta.Deleted _ -> [GV.fontColor GV.Gray, GV.color GV.Gray]
                Ta.Recurring _ _ -> []
              <> case hls `intersect` S.toList (Ta.tags t) of
                [] -> []
                _ -> [GV.fontColor GV.Red, GV.color GV.Red]
        , GV.globalAttributes = [GV.GraphAttrs []]
        }
    )

data RenderOption = RenderOption
  { showDeleted :: Bool
  , highlights :: [Ta.Tag]
  }

tasksToDotImpure :: TimeZone -> RenderOption -> [Task] -> IO T.Text
tasksToDotImpure tz os =
  getClosureImpure
    >>> fmap
      ( (if showDeleted os then id else purgeDeleted)
          >>> tasksClosureToGraph
          >>> taskGraphVis tz (highlights os)
          >>> GV.printDotGraph
          >>> TL.toStrict
      )
