{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module TaskUtils.Renderer.Terminal (
  formatTasks,
  listTask,
  TaskColumn (..),
) where

import Data.List (intersperse, sortBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isNothing)
import Data.Semigroup (Semigroup (..))
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Lazy.IO qualified as TZIO
import Data.UUID qualified as UUID
import System.Console.Terminal.Size qualified as TS
import Taskwarrior.Status (Status (..))
import Taskwarrior.Task (Task (..))
import Text.DocLayout qualified as L

data TaskColumn
  = IdOrUUID
  | Status
  | Description
  | Tags
  | Urg
  deriving (Show)

data TaskColumnRenderer = TaskColumnRenderer
  { label :: L.Doc T.Text
  , picker :: Task -> L.Doc T.Text
  , width :: Int
  }

padFloat :: Int -> Int -> Double -> L.Doc T.Text
padFloat intPartLen floatPartLen f =
  let
    intPart :: Int = floor f
    floatPart = f - fromIntegral intPart
    floatStr = tail $ show floatPart
   in
    L.rblock intPartLen (L.literal $ T.pack $ show intPart)
      <> L.lblock
        (1 + floatPartLen)
        ( L.literal $ T.pack (if length floatStr > floatPartLen + 1 then take (floatPartLen + 1) floatStr else floatStr)
        )

computeRenderer :: [Task] -> TaskColumn -> TaskColumnRenderer
computeRenderer tasks col =
  case col of
    IdOrUUID ->
      let
        lackId = any (\t -> isNothing t.id) tasks
        width = if lackId then 8 else length (show (maximum (map (fromMaybe 0 . (.id)) tasks)))
        label = if lackId then "ID/UUID" else "ID"
        idOrUUIDPicker (t :: Task) =
          L.literal $
            T.pack $
              case t.id of
                Just i -> show i
                Nothing -> take 8 $ UUID.toString t.uuid
       in
        TaskColumnRenderer{label = label, picker = idOrUUIDPicker, width = width}
    Description ->
      let
        width = min 30 (maximum $ map (L.realLength . description) tasks)
       in
        TaskColumnRenderer
          { label = "Desc"
          , picker = L.hsep . map L.literal . T.splitOn " " . (.description)
          , width = width
          }
    Status ->
      TaskColumnRenderer
        { label = "Stat"
        , picker = L.literal . T.pack . (\case Pending -> "o"; Completed _ -> "-"; Deleted _ -> "x"; Recurring _ _ -> "@") . (.status)
        , width = 4
        }
    Tags ->
      let
        width = min 20 (2 * maximum (map (maximum . map T.length . S.toList . (\t -> t.tags)) tasks))
       in
        TaskColumnRenderer
          { label = "Tags"
          , picker = L.hsep . map (L.nowrap . L.literal) . S.toList . (.tags)
          , width = width
          }
    Urg ->
      TaskColumnRenderer
        { label = "Urg"
        , picker = padFloat 2 2 . (.urgency)
        , width = 5
        }

formatTasks :: [TaskColumn] -> [Task] -> L.Doc T.Text
formatTasks cols tasks =
  let
    renderers = map (computeRenderer tasks) cols
    docHeader =
      L.hcat
        ( intersperse
            (L.vfill " ")
            ( map
                ( \renderer ->
                    L.lblock
                      renderer.width
                      (L.underlined $ L.bold renderer.label)
                )
                renderers
            )
        )
   in
    docHeader
      <> L.cr
      <> L.vcat
        ( map
            ( \task ->
                L.hcat $
                  intersperse (L.vfill " ") $
                    map (\render -> L.lblock render.width (render.picker task)) renderers
            )
            tasks
        )
      <> L.cr

listTask :: [TaskColumn] -> [Task] -> IO ()
listTask cols tasks = do
  if null tasks
    then putStrLn "No tasks found"
    else do
      s <- TS.size
      let
        width = do
          (TS.Window w _) <- s
          return w
        ts =
          sortBy
            ( \l r ->
                sconcat (c l.status r.status :| [compare r.urgency l.urgency])
            )
            tasks
         where
          c :: Status -> Status -> Ordering
          c Pending Pending = EQ
          c Pending _ = LT
          c (Recurring _ _) Pending = GT
          c (Recurring _ _) (Recurring _ _) = EQ
          c (Recurring _ _) _ = LT
          c (Completed _) Pending = GT
          c (Completed _) (Recurring _ _) = GT
          c (Completed _) (Completed _) = EQ
          c (Completed _) _ = LT
          c (Deleted _) Pending = GT
          c (Deleted _) (Recurring _ _) = GT
          c (Deleted _) (Completed _) = GT
          c (Deleted _) (Deleted _) = EQ
        doc = formatTasks cols ts
        t = L.renderANSI width doc
      TZIO.putStr t
