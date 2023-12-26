{-# LANGUAGE TupleSections #-}

module Day16 (part1, part2) where

import Data.List.Split (chunksOf)
import Data.Set (Set, empty, insert, map, member)
import GHC.Arr (Array, array, bounds, indices, (!))
import Utils (Direction (..), Position, Vector, withIndex2D)
import Prelude hiding (Left, Right)

-- 6855 Correct :)
part1 :: String -> String
part1 s = show $ countEnergisedTiles tileGrid ((0, 0), Right)
  where
    tileGrid = parseInput s
    _emptyVisitedGrid = array (bounds tileGrid) [(i, False) | i <- indices tileGrid]
    (_, (_w, _h)) = bounds tileGrid

-- 7513 Correct :)
part2 :: String -> String
part2 s = show $ maximum $ Prelude.map (countEnergisedTiles tileGrid) allEdges
  where
    tileGrid = parseInput s
    (_, (w, h)) = bounds tileGrid

    leftEdge = [((i, 0), Right) | i <- [0 .. w]]
    rightEdge = [((i, h), Left) | i <- [0 .. w]]
    topEdge = [((0, i), Down) | i <- [0 .. h]]
    bottomEdge = [((w, i), Up) | i <- [0 .. h]]
    allEdges = leftEdge ++ rightEdge ++ topEdge ++ bottomEdge

data MirrorType = Forward | Backward deriving (Show, Eq, Ord)

data SplitterType = Vertical | Horizontal deriving (Show, Eq, Ord)

data Tile
  = EmptySpace
  | Mirror MirrorType
  | Splitter SplitterType
  deriving (Show, Eq)

type TileGrid = Array Position Tile

type VisitedPoints = Set Vector

parseInput :: String -> Array Position Tile
parseInput s = array ((0, 0), (length grid - 1, length (head grid) - 1)) tileGridData
  where
    grid :: [[Char]]
    grid = lines s

    tileGridData :: [(Position, Tile)]
    tileGridData = concat $ withIndex2D $ Prelude.map (Prelude.map charToTile) grid

    charToTile :: Char -> Tile
    charToTile '/' = Mirror Forward
    charToTile '\\' = Mirror Backward
    charToTile '|' = Splitter Vertical
    charToTile '-' = Splitter Horizontal
    charToTile _ = EmptySpace

visitTiles :: TileGrid -> VisitedPoints -> Vector -> Int -> VisitedPoints
visitTiles tiles visited vec@(pos, dir) depth
  -- Already done, don't loop
  | vec `member` visited = visited
  -- Move onwards!
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
  | otherwise = error $ "Unknown input " ++ show vec ++ "\n" ++ _showVisitedPoints (snd $ bounds tiles) visited
  where
    currTile = tiles ! pos

    nextVisited = vec `insert` visited
    (_, (w, h)) = bounds tiles

    movePos :: Position -> Direction -> Maybe Position
    movePos (a, b) Up = if a > 0 then Just (a - 1, b) else Nothing
    movePos (a, b) Down = if a < w then Just (a + 1, b) else Nothing
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

_showVisitedPoints :: (Int, Int) -> VisitedPoints -> String
_showVisitedPoints (w, h) vectors =
  unlines $ chunksOf (w + 1) [if (i, j) `member` points then '#' else '.' | (i, j) <- concatMap (\i -> Prelude.map (i,) [0 .. h]) [0 .. w]]
  where
    points = Data.Set.map fst vectors

countEnergisedTiles :: TileGrid -> Vector -> Int
countEnergisedTiles tileGrid v = length $ Data.Set.map fst $ visitTiles tileGrid Data.Set.empty v 10000000