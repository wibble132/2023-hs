{-# LANGUAGE TupleSections #-}

module Day21 (part1, part2) where

import Data.List (elemIndex)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust, mapMaybe)
-- import Debug.Trace (trace)
import GHC.Arr (Array, array, assocs, bounds, elems, (!), (//))
import Utils (Direction (..), Position)
import Prelude hiding (Left, Right)

part1 :: String -> String
part1 = show . length . filter part1Filter . elems . doSearchFromS 64 . parseInput

part1Filter :: Tile -> Bool
part1Filter Wall = False
part1Filter (Space i) = i `mod` 2 == 1

part2 :: String -> String
part2 = _showGrid . flip goSearch [((0,0),256)] . parseInput

data Tile = Wall | Space Int deriving (Show, Eq)

type Grid = Array Position Tile

-- Assumes the input is square
parseInput :: String -> Grid
parseInput s = array ((0, 0), (w, w)) . concat . zipWith (\i -> zipWith (\j b -> ((i, j), b)) [0 ..]) [0 ..] . map (map charToTile) . lines $ s
  where
    w = fromJust ('\n' `elemIndex` s) - 1

charToTile :: Char -> Tile
charToTile '#' = Wall
charToTile 'S' = Space (-1)
charToTile _ = Space 0

spaceValue :: Tile -> Int
spaceValue Wall = error "Can't take spaceValue of a Wall"
spaceValue (Space i) = i

_showGrid :: Grid -> String
_showGrid a = unlines . map (concatMap tileToChar) . chunksOf (w + 1) $ elems a
  where
    (_, (w, _)) = bounds a

    tileToChar :: Tile -> String
    tileToChar Wall = " # "
    tileToChar (Space 0) = " . "
    tileToChar (Space i) = pad 3 ' ' $ show i

pad :: Int -> a -> [a] -> [a]
pad i c (s : ss) = s : pad (i - 1) c ss
pad i c [] = replicate i c

doSearchFromS :: Int -> Grid -> Grid
doSearchFromS searchRange grid = goSearch grid startPos
  where
    startPos = map ((,searchRange + 1) . fst) $ filter (\(_, x) -> x /= Wall && spaceValue x < 0) $ assocs grid

--          map
goSearch :: Grid -> [(Position, Int)] -> Grid
goSearch g [] = g
goSearch g ((p, i) : rest)
--   | trace (show neighbours) False = undefined
    | g ! p == Wall = goSearch g rest
    | spaceValue (g ! p) >= i = goSearch g rest
    | otherwise = goSearch (g // [(p, Space i)]) (rest ++ neighbours)
    where
    (_, (w, h)) = bounds g
    neighbours = map (,i - 1) $ mapMaybe (movePos p) [Up, Down, Left, Right]

    movePos :: Position -> Direction -> Maybe Position
    movePos (a, b) Up = if a > 0 then Just (a - 1, b) else Nothing
    movePos (a, b) Down = if a < w then Just (a + 1, b) else Nothing
    movePos (a, b) Left = if b > 0 then Just (a, b - 1) else Nothing
    movePos (a, b) Right = if b < h then Just (a, b + 1) else Nothing