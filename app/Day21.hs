{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Day21 (part1, part2) where

import Data.List (elemIndex, findIndex, group)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust, mapMaybe, fromMaybe)
-- import Debug.Trace (trace)
import GHC.Arr (Array, array, assocs, bounds, elems, (!), (//))
import Utils (Direction (..), Position)
import Prelude hiding (Left, Right)
import qualified Data.Map as Map

part1 :: String -> String
part1 = show . length . filter part1Filter . elems . doSearchFromS 64 . parseInput

part1Filter :: Tile -> Bool
part1Filter Wall = False
part1Filter (Space i) = i `mod` 2 == 1

-- It's 130 steps to go from S to a corner
-- It's 130 steps to go from one corner to either adjacent corner
-- It's 260 steps to go from one corner to the opposite corner
--
-- So the corners are:
--  261 .. 131
--   :      :
--  131 ..  1
--
-- And from S:
--  1  ..  66 ..  1
--  :      :      :
--  66 .. 131 ..  66
--  :      :      :
--  1  ..  66 ..  1
--
-- 392

-- doSearchFromPos 261 puts 1 in top left corner of map to the left of the centre map
part2 :: String -> String
-- part2 s = show $ countEndpoints $ flip goSearch [((196, 196), 197)] $ parseInput tripleInput -- 32808
-- part2 s = show $ countEndpoints $ doSearchFromS (steps + 1) grid
-- part2 s = show (distCornerToS, sideLength, fullMapsToLeft, leftStartNum, halfSide, remainingEdgeDist) --edgeValues, vertexValues, interiorValues)
-- part2 s = show (edgeValues + vertexValues + interiorValues)
-- 594115392422962 too high (Included repeats of S as an endpoint)
-- 594115391613762 too high -- not sure why...
part2 s =
  show $
    (n * n) * fullS -- 7255 -- fullS
      + (n * n + 2 * n + 1) * fullNotS -- 7262 -- fullNotS
      + 4 * sum midPoints --(5467 + 5474 + 5457 + 5464) -- sum midPoints
      + (n + 1) * sum smallCorners -- (917 + 935 + 919 + 913) -- sum smallCorners
      + n * sum bigCorners -- (6351 + 6371 + 6361 + 6358) -- sum bigCorners
-- part2 s = show (fullNotS + sum midPoints + sum smallCorners) -- 32801
-- part2 s = unlines $ map show [smallCorners, bigCorners, midPoints, [fullS, fullNotS, n]]
-- -- part2 s =
-- --   unlines $
-- --     map
-- --       (\(a, b, c) -> a ++ b ++ c)
-- --       ( zip3
-- --           (lines $ _showGrid (goSearch grid [((w, h), 65)]))
-- --           (lines $ _showGrid (goSearch grid [((w, halfSide), 131)]))
-- --           (lines $ _showGrid (goSearch grid [((w, 0), 65)]))
-- --       )
  where
    tripleInput = unlines $ (\x -> x ++ x ++ x) $ map (\x -> x ++ x ++ x) $ reverse $ lines s

    n :: Integer
    n = 202299
    -- n = 0

    grid = parseInput $ unlines $ reverse $ lines s
    initialSearch = doSearchFromS 500 grid
    (_, (w, h)) = bounds initialSearch
    corners = [initialSearch ! (0, 0), initialSearch ! (0, h), initialSearch ! (w, 0), initialSearch ! (w, h)]
    distCornerToS = if allSame corners then 501 - spaceValue (head corners) else error $ "Not all the corners are the same distance away: " ++ show corners

    sideLength = if allSame [w, h] then w + 1 else error "Input not square"

    steps = 26501365
    -- steps = 260
    -- steps = 5
    fullMapsToLeft = (steps - distCornerToS) `div` sideLength -- Due to symmetries == full maps in each direction
    halfSide = w `div` 2

    smallCorners = map (\p -> countEndpoints $ goSearch grid [(p, 65)]) [(0, 0), (w, 0), (0, h), (w, h)]
    bigCorners = map (\p -> countEndpoints $ goSearch grid [(p, 196)]) [(0, 0), (w, 0), (0, h), (w, h)]
    midPoints = map (\p -> countEndpoints $ goSearch grid [(p, 131)]) [(0, halfSide), (halfSide, 0), (w, halfSide), (halfSide, h)]
    fullS = countEndpoints $ doSearchFromS steps grid
    fullNotS = countEndpoints $ doSearchFromS (steps + 1) grid

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
    startPos = (: []) . takeMiddle . map ((,searchRange + 1) . fst) $ filter (\(_, x) -> x /= Wall && spaceValue x < 0) $ assocs grid

takeMiddle :: [a] -> a
takeMiddle [] = undefined
takeMiddle [a] = a
takeMiddle [a, _] = a
takeMiddle l = takeMiddle (tail . init $ l)

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
    !neighbours =
      -- filter (\(p', i') -> g ! p' /= Wall && spaceValue (g ! p') < i') . --oddly this slows things down, probably because it has to create a copy of g and hold it until used
      map (,i - 1) $
        mapMaybe (movePos p) [Up, Down, Left, Right]

    movePos :: Position -> Direction -> Maybe Position
    movePos (a, b) Up = if a > 0 then Just (a - 1, b) else Nothing
    movePos (a, b) Down = if a < w then Just (a + 1, b) else Nothing
    movePos (a, b) Left = if b > 0 then Just (a, b - 1) else Nothing
    movePos (a, b) Right = if b < h then Just (a, b + 1) else Nothing

allSame :: (Eq a) => [a] -> Bool
allSame (x : y : rest)
  | x == y = allSame $ y : rest
  | otherwise = False
allSame [_] = True
allSame [] = True

countEndpoints :: Grid -> Integer
countEndpoints = toInteger . length . filter isSourceEndpoint . elems

isSourceEndpoint :: Tile -> Bool
isSourceEndpoint Wall = False
isSourceEndpoint (Space i)
  | i <= 0 = False
  | otherwise = i `mod` 2 == 1

countUpperEndpoints :: Grid -> Integer
countUpperEndpoints = toInteger . length . filter isSourceEndpoint . map snd . filter (isUpperGrid . fst) . assocs
  where
    isUpperGrid (x, y) = 262 <= x && y < 131
