{-# LANGUAGE TupleSections #-}

module Day17 (part1, part2) where

import Algorithm.Search (dijkstra)
import Data.Char (digitToInt, intToDigit)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import GHC.Arr (Array, array, assocs, bounds, (!))
import Utils (Direction (..), Position, Vector, isOpposite, withIndex2D, isAdjacent)
import Prelude hiding (Left, Right)

-- 681 too high -- Started in bad direction
-- 677 too high -- Didn't reset step count when turning before 3 steps
-- 665 Correct :)
part1 :: String -> String
-- part1 = (\((i, p), m) -> show i ++ "\n" ++ _showPath m p) . (\m -> (,m) $ fromJust (search' m)) . parseInput
part1 = show . fst . fromJust . search' . parseInput

-- 810 too high -- Didn't slow down at end (& something else?)
-- 664 too low -- Not sure what I changed? Exported `isAdjacent` but that shouldn't be broken? But it is going in very long lines now so maybe...
part2 :: String -> String
part2 = (\((i, p), m) -> show i ++ "\n" ++ _showPath m p) . (\m -> (,m) $ fromJust (ultraSearch' m)) . parseInput

-- part2 = show . fst . fromJust . ultraSearch' . parseInput

type Map = Array (Int, Int) Int

parseInput :: String -> Map
parseInput s = array ((0, 0), (length mappedInput - 1, length (head mappedInput) - 1)) mapData
  where
    mappedInput :: [[Int]]
    mappedInput = map (map digitToInt) $ lines s

    mapData :: [((Int, Int), Int)]
    mapData = concat $ withIndex2D mappedInput

type State = (Vector, Int)

search' :: Map -> Maybe (Int, [State])
search' m = dijkstra getValidNeighbours getCost isFinish (((0, 0), Right), 0)
  where
    getValidNeighbours :: State -> [State]
    -- turn left or right only
    getValidNeighbours ((pos, dir), 3) = map (,1) $ filter ((/= dir) . snd) $ filter (not . isOpposite dir . snd) $ getNeighbours pos
    -- turn left or right, or continue forwards
    getValidNeighbours ((pos, dir), i) = map (\v@(_, d) -> if d == dir then (v, i + 1) else (v, 1)) $ filter (not . isOpposite dir . snd) $ getNeighbours pos

    getNeighbours :: Position -> [Vector]
    getNeighbours pos = mapMaybe (movePos pos) [Up, Down, Left, Right]

    getCost :: State -> State -> Int
    getCost _ ((p2, _), _) = m ! p2

    isFinish :: State -> Bool
    isFinish ((p2, _), _) = p2 == bnds

    (_, bnds@(w, h)) = bounds m
    movePos :: Position -> Direction -> Maybe Vector
    movePos (a, b) Up = if a > 0 then Just ((a - 1, b), Up) else Nothing
    movePos (a, b) Down = if a < w then Just ((a + 1, b), Down) else Nothing
    movePos (a, b) Left = if b > 0 then Just ((a, b - 1), Left) else Nothing
    movePos (a, b) Right = if b < h then Just ((a, b + 1), Right) else Nothing

ultraSearch' :: Map -> Maybe (Int, [State])
ultraSearch' m = dijkstra getValidNeighbours getCost isFinish (((0, 0), Down), 0) -- Should run this with Down or Right cuz can't change dir at first...
  where
    getValidNeighbours :: State -> [State]
    getValidNeighbours ((pos, dir), i)
      -- continue forwards only
      | i <= 3 = map (,i + 1) $ filter ((== dir) . snd) $ getNeighbours pos
      -- turn left or right, or continue forwards
      | i <= 9 = map (\v@(_, d) -> if d == dir then (v, i + 1) else (v, 1)) $ filter (not . isOpposite dir . snd) $ getNeighbours pos
      -- turn left or right only
      | i == 10 = map (,1) $ filter (isAdjacent dir . snd) $ getNeighbours pos
      | otherwise = error "Bad"

    getNeighbours :: Position -> [Vector]
    getNeighbours pos = mapMaybe (movePos pos) [Up, Down, Left, Right]

    getCost :: State -> State -> Int
    getCost _ ((p2, _), _) = m ! p2

    isFinish :: State -> Bool
    isFinish ((p2, _), i) = p2 == bnds && i >= 4

    (_, bnds@(w, h)) = bounds m
    movePos :: Position -> Direction -> Maybe Vector
    movePos (a, b) Up = if a > 0 then Just ((a - 1, b), Up) else Nothing
    movePos (a, b) Down = if a < w then Just ((a + 1, b), Down) else Nothing
    movePos (a, b) Left = if b > 0 then Just ((a, b - 1), Left) else Nothing
    movePos (a, b) Right = if b < h then Just ((a, b + 1), Right) else Nothing

_showPath :: Map -> [State] -> String
_showPath m s = unlines $ chunksOf (h + 1) $ map replaceWithPath $ assocs m
  where
    (_, (_, h)) = bounds m

    replaceWithPath :: (Position, Int) -> Char
    replaceWithPath (p, i) = fromMaybe (intToDigit i) $ firstInPath p

    dirToChar :: Direction -> Char
    dirToChar Up = '^'
    dirToChar Down = 'v'
    dirToChar Left = '<'
    dirToChar Right = '>'

    firstInPath :: Position -> Maybe Char
    firstInPath p = case length points of
      0 -> Nothing
      1 -> Just $ (\((_, d), _) -> dirToChar d) $ head points
      _ -> error "bad"
      where
        points = filter (\((p', _), _) -> p' == p) s