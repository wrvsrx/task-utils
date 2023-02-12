{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Task (
  taskJsonToDotPure,
  taskJsonToDotImpure,
) where

import Control.Exception (assert)
import Data.Aeson qualified as A
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Graph.Inductive (Gr, Node, mkGraph)
import Data.GraphViz qualified as GV
import Data.List (intersect)
import Data.Map qualified as M
import Data.Maybe
import Data.Set ((\\))
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time qualified as Time
import Data.UUID (UUID)
import Data.UUID qualified as UU
import Taskwarrior.IO qualified as Ta
import Taskwarrior.Status qualified as Ta
import Taskwarrior.Task (Task)
import Taskwarrior.Task qualified as Ta

taskDeserialize :: BL.ByteString -> [Task]
taskDeserialize = fromMaybe (error "fail to parse json") . (A.decode :: BL.ByteString -> Maybe [Task])

-- getEdgesOfTask :: M.Map Text Int -> Node -> (Task, Node) -> [(Node, Node)]
-- getEdgesOfTask m failNode (Task _ _ deps _ _ _, n) = map ((n,) . fromMaybe failNode . (`M.lookup` m)) deps

getDepsNodeOfATask :: M.Map UUID Node -> Task -> ([Node], [UUID])
getDepsNodeOfATask m t =
  let
    depsUUID = Ta.depends t & S.toList
    depsNode = mapMaybe (`M.lookup` m) depsUUID
    nonExistUUID = filter (\x -> case x `M.lookup` m of Just _ -> False; Nothing -> True) depsUUID
   in
    (depsNode, nonExistUUID)

getEdgesOfTaskInClosure :: M.Map UUID Int -> (Task, Node) -> [(Node, Node)]
getEdgesOfTaskInClosure m (t, n) =
  let
    res = getDepsNodeOfATask m t
   in
    assert (null (snd res)) res & fst & map (n,)

getClosurePure :: [Task] -> [Task]
getClosurePure ts =
  let
    tasksUUIDSet = S.fromList (map Ta.uuid ts)
    depsUUIDSet = mconcat (map Ta.depends ts)
    outsideDepsUUIDSet = depsUUIDSet \\ tasksUUIDSet
    zeroUTC = Time.UTCTime (Time.ModifiedJulianDay 0) (Time.secondsToDiffTime 0)
    outsideTasks = map (\x -> Ta.makeTask x zeroUTC "outside") (S.toList outsideDepsUUIDSet)
   in
    ts <> outsideTasks

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

tasksClosureToGraph :: [Task] -> Gr Task ()
tasksClosureToGraph ts =
  let
    tasksWithNode = ts `zip` [(1 :: Node) ..]
    mapFromUUIDToNode = tasksWithNode & map (first Ta.uuid) & M.fromList
    nodes = tasksWithNode & map (\(x, y) -> (y, x))
    edges =
      tasksWithNode
        & concatMap (getEdgesOfTaskInClosure mapFromUUIDToNode)
        & map (\(x, y) -> (x, y, ()))
    graph = mkGraph nodes edges :: Gr Task ()
   in
    graph

taskGraphVis :: [Ta.Tag] -> Gr Task () -> GV.DotGraph Node
taskGraphVis hls =
  GV.graphToDot
    ( GV.nonClusteredParams
        { GV.fmtNode = \(_, t) ->
            [ GV.toLabel $
                Ta.description t
                  <> "\n"
                  <> maybe "" ((<> ",") . T.pack . show) (Ta.id t)
                  <> T.take 8 (UU.toText (Ta.uuid t))
            ]
              <> case Ta.status t of
                Ta.Completed _ -> [GV.fontColor GV.Gray]
                Ta.Pending -> []
                Ta.Deleted _ -> []
                Ta.Recurring _ _ -> []
              <> case hls `intersect` S.toList (Ta.tags t) of
                [] -> []
                _ -> [GV.color GV.Red]
        , GV.globalAttributes = [GV.GraphAttrs []]
        }
    )

taskJsonToDotPure :: [Ta.Tag] -> BL.ByteString -> T.Text
taskJsonToDotPure tags bs =
  bs
    & taskDeserialize
    & getClosurePure
    & tasksClosureToGraph
    & taskGraphVis tags
    & GV.printDotGraph
    & TL.toStrict

taskJsonToDotImpure :: [Ta.Tag] -> BL.ByteString -> IO T.Text
taskJsonToDotImpure tags bs =
  bs
    & taskDeserialize
    & getClosureImpure
    <&> tasksClosureToGraph
    <&> taskGraphVis tags
    <&> GV.printDotGraph
    <&> TL.toStrict
