module Day18 (part1, part2) where

import Data.Char (digitToInt)
import Data.Foldable (foldl')
import Data.List (elemIndex, group, sort, sortOn)
import Data.Maybe (fromJust)
import Utils (Direction (Down, Left, Right, Up))
import Prelude hiding (Left, Right)
import Data.Ratio ((%))

type Position = (Integer, Integer)

-- 52035 Correct :)
part1 :: String -> String
part1 = show . areaWithin . parseInput

-- 60612102040578 too high
-- 60612092439765 Using shoelace + Pick's
part2 :: String -> String
part2 = show . clearedTiles . verts . parseInput2

charToDirection :: Char -> Direction
charToDirection 'U' = Up
charToDirection 'D' = Down
charToDirection 'L' = Left
charToDirection 'R' = Right
charToDirection c = error $ "Bad Direction " ++ [c]

type Instruction = (Direction, Integer)

parseInput :: String -> [Instruction]
parseInput = map parseLine . lines

parseLine :: String -> Instruction
parseLine = f . take 2 . words
  where
    f :: [String] -> Instruction
    f ((a : _) : b : _) = (charToDirection a, read b)
    f _ = error "Bad input line"

verts :: [Instruction] -> [Position]
verts i = vertsGen i (0, 0)
  where
    vertsGen :: [Instruction] -> Position -> [Position]
    vertsGen (i' : is) p = p : vertsGen is (move i' p)
    vertsGen [] _ = []

getBounds :: [Instruction] -> (Position, Position)
getBounds i = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    xs :: [Integer]
    xs = map fst (verts i)
    ys :: [Integer]
    ys = map snd (verts i)

move :: Instruction -> Position -> Position
move (dir, dist) (x, y) = case dir of
  Up -> (x - dist, y)
  Down -> (x + dist, y)
  Left -> (x, y - dist)
  Right -> (x, y + dist)

moves :: Instruction -> Position -> [Position]
moves (dir, dist) (x, y) = case dir of
  Up -> [(x - i, y) | i <- [1 .. dist]]
  Down -> [(x + i, y) | i <- [1 .. dist]]
  Left -> [(x, y - i) | i <- [1 .. dist]]
  Right -> [(x, y + i) | i <- [1 .. dist]]

verticalWalls :: [Instruction] -> Position -> [Position]
verticalWalls (i : is) p
  | fst i == Up = moves i p ++ verticalWalls is (move i p)
  | fst i == Down = (p : init (moves i p)) ++ verticalWalls is (move i p)
  | fst i == Left || fst i == Right = verticalWalls is (move i p)
  | otherwise = []
verticalWalls [] _ = []

-- Somewhat naive method, works fine on smaller sizes by counting points inside one at a time
areaWithin :: [Instruction] -> Integer
areaWithin is = (toInteger (length allWalls) - 1) + sum (map processRow [x1 .. x2])
  where
    verticals = verticalWalls is (0, 0)
    ((x1, y1), (x2, y2)) = getBounds is

    allWalls = foldl' (\acc i -> acc ++ moves i (last acc)) [(0, 0)] is

    processRow :: Integer -> Integer
    processRow x = fst $ foldl' (processPoint x) (0, False) [y1 .. y2]

    processPoint :: Integer -> (Integer, Bool) -> Integer -> (Integer, Bool)
    processPoint x (acc, ins) y
      | (x, y) `elem` verticals = (acc, not ins)
      | (x, y) `elem` allWalls = (acc, ins)
      | otherwise = if ins then (acc + 1, ins) else (acc, ins)

-- areaWithin' :: [Position] -> Int

parseInput2 :: String -> [Instruction]
parseInput2 = map (hexToInstruction . take 6 . drop 2 . (!! 2) . words) . lines

hexToInstruction :: String -> Instruction
hexToInstruction s = (digitToDirection $ last s, foldl' (\acc x -> 16 * acc + toInteger (digitToInt x)) 0 (take 5 s))

digitToDirection :: Char -> Direction
digitToDirection '0' = Right
digitToDirection '1' = Down
digitToDirection '2' = Left
digitToDirection '3' = Up
digitToDirection _ = error ""

type VerticalLine = (Integer, Integer, Integer)

-- Calculate only on the rows with a vertex in, and the rows below that
--  multiply those "below" rows by the distance to the next vertex (since those all the same)
-- Each row can be calculated by considering the vertical walls and the intervals between them
--   Hmm also want to remove the regions for horizontal walls?
---- DOESN'T WORK, THIS IS VERY SAD. USED SHOELACE FORMULA BELOW AND WAS CORRECT
_areaWithin2 :: [Position] -> Integer
_areaWithin2 ps = perimeter ps + sum processedRows
  where
    vertexRows :: [Integer]
    vertexRows = tail $ map head $ group $ sort $ concatMap (\x -> [x, x + 1]) $ init $ sort $ map fst ps

    vertexRowSizes :: [(Integer, Integer)]
    vertexRowSizes = map (\(y1, y2) -> (y1, y2 - y1)) $ withPairs vertexRows

    verticals :: [VerticalLine]
    verticals =
      map fixPairOrder $
        sortOn (\(_, _, y) -> y) $
          map (\((x1, y1), (x2, _)) -> (x1, x2, y1)) $
            filter
              (\((_, y1), (_, y2)) -> y1 == y2)
              (withPairs ps ++ [(last ps, head ps)])

    fixPairOrder :: VerticalLine -> VerticalLine
    fixPairOrder (x1, x2, y)
      | x1 < x2 = (x1, x2, y)
      | otherwise = (x2, x1, y)

    inVertical :: Integer -> VerticalLine -> Bool
    inVertical x (x1, x2, _) = x1 <= x && x < x2

    processRow :: Integer -> Integer
    processRow x = (\(a, _, _) -> a) $ foldl' (processRowStep x) (0, False, (0, 0, -100)) verticals

    areNeighbours :: Position -> Position -> Bool
    areNeighbours p1 p2 = (abs (i1 - i2) == 1) || (abs (i1 - i2) == length ps - 1)
      where
        i1 = fromJust $ p1 `elemIndex` ps
        i2 = fromJust $ p2 `elemIndex` ps

    processRowStep :: Integer -> (Integer, Bool, VerticalLine) -> VerticalLine -> (Integer, Bool, VerticalLine)
    processRowStep x (tot, ins, lastVert@(lastX1, lastX2, lastY)) vert@(x1, x2, y)
      -- Don't contact this vert at all, continue
      | x2 < x || x1 > x = (tot, ins, lastVert)
      -- If we weren't already inside
      | not ins && not (x `inVertical` vert) = (tot, ins, lastVert)
      | not ins = (tot, True, vert)
      -- If we are already inside
      -- Don't increase if we are on a line, but note we hit this vertical
      | x == lastX2 && x == x1 && areNeighbours (lastX2, lastY) (x1, y) = (tot, False, vert)
      | x == lastX1 && x == x2 && areNeighbours (lastX1, lastY) (x2, y) = (tot, True, vert)
      -- If we inside and hit the bottom of this, make note of the vert & increase count but don't change in/out
      | x == x2 = (tot + y - lastY - 1, True, vert)
      -- If not in the vertical, ignore this vert
      | not (x `inVertical` vert) = (tot, True, lastVert)
      -- In the vertical, add up
      | otherwise = (tot + y - lastY - 1, False, vert)

    processedRows :: [Integer]
    processedRows = map (\(x, size) -> size * processRow x) vertexRowSizes

perimeter :: [Position] -> Integer
perimeter ps = foldl' (+) 0 lengths
  where
    l = length ps
    pairedVertices = take l $ withPairs $ cycle ps
    lengths = map (\((x1, y1), (x2, y2)) -> abs (x1 - x2) + abs (y1 - y2)) pairedVertices

withPairs :: [a] -> [(a, a)]
withPairs (a : c@(b : _)) = (a, b) : withPairs c
withPairs [_] = []
withPairs [] = []

-- Like withPairs but includes a final element of (last, first)
withPairsCycled :: [a] -> [(a, a)]
withPairsCycled l = take (length l) $ withPairs $ cycle l

-- Area of a polygon with vertices (x1,y1)...(xn,yn) is
--- 1/2 * sum_{i=1}^n xi * (y{i+1} - yi) (taking y{n+1} = y1)
shoelace :: [Position] -> Rational
shoelace = (% 2) . abs . sum . map (\((xi, yi), (xi1, yi1)) -> xi * yi1 - yi * xi1) . withPairsCycled

-- By Pick's theorem, Area = [# Inside Points] + [# boundary points] - 1
clearedTiles :: [Position] -> Integer
clearedTiles ps = round (a - (b % 2)) + 1 + b
  where
    a = shoelace ps
    b = perimeter ps