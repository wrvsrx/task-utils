{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Task (
  taskJsonToDot,
) where

-- taskJsonToDot,

import Cli
import Data.Aeson qualified as A
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Graph.Inductive (Gr, Node, mkGraph)
import Data.GraphViz qualified as GV
import Data.List (intersect)
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.UUID (UUID)
import Data.UUID qualified as UU
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

getEdgesOfTaskDropOutside :: M.Map UUID Int -> (Task, Node) -> [(Node, Node)]
getEdgesOfTaskDropOutside m (t, n) = getDepsNodeOfATask m t & fst & map (n,)

taskJsonToDot :: VisOption -> BL.ByteString -> T.Text
taskJsonToDot vs bs =
  let tasksWithNode =
        bs
          & taskDeserialize
          & (`zip` [(1 :: Node) ..])
      mapFromUUIDToNode = tasksWithNode & map (first Ta.uuid) & M.fromList
      nodes = tasksWithNode & map (\(x, y) -> (y, x))
      edges =
        tasksWithNode
          & concatMap (getEdgesOfTaskDropOutside mapFromUUIDToNode)
          & map (\(x, y) -> (x, y, ()))
      graph = mkGraph nodes edges :: Gr Task ()
   in (TL.toStrict . GV.printDotGraph) $ taskGraphVis (highlights vs) graph

taskGraphVis :: [T.Text] -> Gr Task () -> GV.DotGraph Node
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
