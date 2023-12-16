{-# LANGUAGE TupleSections #-}

module Day16 (part1, part2) where

import Data.List.Split (chunksOf)
import GHC.Arr (Array, array, bounds, elems, indices, (!))
import Utils (mapIndexed)
import Prelude hiding (Left, Right)
import Data.Set (Set, insert, member, map, empty)

-- 6855 Correct :)
part1 :: String -> String
-- part1 s = _showVisitedPoints w h $ visitTiles tileGrid Data.Set.empty ((0, 0), Right) 200
part1 s = show $ length $ Data.Set.map fst $ visitTiles tileGrid Data.Set.empty ((0, 0), Right) 1000000
  where
    tileGrid = parseInput s
    _emptyVisitedGrid = array (bounds tileGrid) [(i, False) | i <- indices tileGrid]
    (_, (_w, _h)) = bounds tileGrid

part2 :: String -> String
part2 = const ""

data MirrorType = Forward | Backward deriving (Show, Eq, Ord)

data SplitterType = Vertical | Horizontal deriving (Show, Eq, Ord)

data Tile
  = EmptySpace
  | Mirror MirrorType
  | Splitter SplitterType
  deriving (Show, Eq)

data Direction = Up | Down | Left | Right deriving (Show, Eq, Ord)

type Position = (Int, Int)

type Vector = (Position, Direction)

type TileGrid = Array Position Tile

type VisitedGrid = Array Position Bool

type VisitedPoints = Set Vector

parseInput :: String -> Array Position Tile
parseInput s = array ((0, 0), (length grid - 1, length (head grid) - 1)) tileGridData
  where
    grid :: [[Char]]
    grid = lines s

    tileGridData :: [(Position, Tile)]
    tileGridData = concat $ mapIndexed (\i -> mapIndexed (\j -> ((i, j),) . charToTile)) grid

    charToTile :: Char -> Tile
    charToTile '/' = Mirror Forward
    charToTile '\\' = Mirror Backward
    charToTile '|' = Splitter Vertical
    charToTile '-' = Splitter Horizontal
    charToTile _ = EmptySpace

visitTiles :: TileGrid -> VisitedPoints -> Vector -> Int -> VisitedPoints
visitTiles tiles visited _v@(pos, dir) depth
  -- Break condition, end on first return to (0,0)
  | pos == (0, 0) && not (null visited) = visited -- visited ! pos = visited
  | _v `member` visited = visited
  | depth == 0 = error $ "Max depth reached " ++ show _v ++ "\n" ++ _showVisitedPoints 9 9 visited
  | currTile == EmptySpace = move dir
  | currTile == Mirror Forward = case dir of
      Up -> move Right
      Down -> move Left
      Left -> move Down
      Right -> move Up
  | currTile == Mirror Backward = case dir of
      Up -> move Left
      Down -> move Right
      Left -> move Up
      Right -> move Down
  | currTile == Splitter Vertical = case dir of
      Up -> move Up
      Down -> move Down
      Left -> moveBoth Up Down
      Right -> moveBoth Up Down
  | currTile == Splitter Horizontal = case dir of
      Up -> moveBoth Left Right
      Down -> moveBoth Left Right
      Left -> move Left
      Right -> move Right
  | otherwise = error $ "Unknown input " ++ show (pos, dir)
  where
    currTile = tiles ! pos

    nextVisited = _v `insert` visited -- // [(pos, True)]
    (_, (w, h)) = bounds tiles

    movePos :: Position -> Direction -> Maybe Position
    movePos (a, b) Up = if a > 0 then Just (a - 1, b) else Nothing
    movePos (a, b) Down = if a < w then Just (a + 1, b) else Nothing -- error $ "Hi" ++ show _v
    movePos (a, b) Left = if b > 0 then Just (a, b - 1) else Nothing
    movePos (a, b) Right = if b < h then Just (a, b + 1) else Nothing

    move :: Direction -> VisitedPoints
    move dir1 = case movePos pos dir1 of
      Just newPos -> visitTiles tiles nextVisited (newPos, dir1) (depth - 1)
      Nothing -> nextVisited

    moveBoth :: Direction -> Direction -> VisitedPoints
    moveBoth dir1 dir2 = case movePos pos dir2 of
      Just newPos -> visitTiles tiles (move dir1) (newPos, dir2) (depth - 1)
      Nothing -> move dir1

_showVisitedGrid :: VisitedGrid -> String
_showVisitedGrid visited = unlines $ chunksOf (w + 1) $ Prelude.map (\x -> if x then '#' else '.') $ GHC.Arr.elems visited
  where
    (_, (_, w)) = bounds visited

_showVisitedPoints :: Int -> Int -> VisitedPoints -> String
_showVisitedPoints w h vectors =
  unlines $ chunksOf (w + 1) [if (i, j) `member` points then '#' else '.' | (i, j) <- concatMap (\i -> Prelude.map (i,) [0 .. h]) [0 .. w]]
  where points = Data.Set.map fst vectors