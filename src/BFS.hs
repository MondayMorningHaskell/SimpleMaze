module BFS where

import           Control.Monad.State
import qualified Data.Array as A
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import qualified Data.Sequence as S
import qualified Data.Set as Set

type Location = (Int, Int)
data Cell = Empty | Wall
  deriving (Eq)
type Grid = A.Array Location Cell

bfsSearch :: Grid -> Location -> Location -> [Location]
bfsSearch grid start finish = evalState (bfsSearch' grid finish) initialState
  where
    initialState = BFSState (S.singleton start) Set.empty M.empty

data BFSState = BFSState
  { queue :: S.Seq Location
  , visited :: Set.Set Location
  , parents :: M.Map Location Location
  } deriving (Show)

bfsSearch' :: Grid -> Location -> State BFSState [Location]
bfsSearch' grid finish = do
  (BFSState q v p) <- get
  case S.viewl q of
    (top S.:< rest) -> if top == finish
      then return (unwindPath p [finish])
      else do
        let validAdjacent = getValidNeighbors top grid v
        let newQueue = foldr (flip (S.|>)) rest validAdjacent
        let newVisited = Set.insert top v
        let newParentsMap = foldr (\loc -> M.insert loc top) p validAdjacent
        put (BFSState newQueue newVisited newParentsMap)
        bfsSearch' grid finish
    _ -> return []

unwindPath :: M.Map Location Location -> [Location] -> [Location]
unwindPath parentsMap currentPath = case M.lookup (head currentPath) parentsMap of
  Nothing -> tail currentPath
  Just parent -> unwindPath parentsMap (parent : currentPath)

getValidNeighbors :: Location -> Grid -> Set.Set Location -> [Location]
getValidNeighbors (r, c) grid v = catMaybes [right', down', left', up']
  where
    (rowMax, colMax) = snd . A.bounds $ grid
    right = (r, c + 1)
    right' = if c + 1 <= colMax && grid A.! right == Empty && not (Set.member right v)
      then Just right
      else Nothing
    down = (r + 1, c)
    down' = if r + 1 <= rowMax && grid A.! down == Empty && not (Set.member down v)
      then Just down
      else Nothing
    left = (r, c - 1)
    left' = if c - 1 >= 0 && grid A.! left == Empty && not (Set.member left v)
      then Just left
      else Nothing
    up = (r - 1, c)
    up' = if r - 1 >= 0 && grid A.! up == Empty && not (Set.member up v)
      then Just up
      else Nothing
