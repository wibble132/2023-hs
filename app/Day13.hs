module Day13 (part1, part2) where

import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Bits (popCount, xor)

-- 21667 too low
-- 32723 Correct :) (one off error in find mirror point missing e.g. [1,2,3,3])
part1 :: String -> String
part1 s = show $ sum $ map scoreGrid1 input
  where
    input = readInput s

-- 34536 Correct :)
part2 :: String -> String
part2 s = show $ sum $ map scoreGrid2 input
  where
    input = readInput s


type Grid = [[Bool]]

readInput :: String -> [Grid]
readInput = map (map (map (== '.')) . lines) . splitOn "\n\n"

rows :: Grid -> [Integer]
rows = map rowToInt

rowToInt :: [Bool] -> Integer
rowToInt = foldl' (\acc x -> 2 * acc + (if x then 1 else 0)) 0

transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose x = map head x : transpose (map tail x)

columns :: Grid -> [Integer]
columns = rows . transpose

findMirrorPoint :: [Integer] -> Maybe Integer
findMirrorPoint l = head' $ map toInteger $ filter (\i -> take i l `isMirror` drop i l) [1 .. length l - 1]

head' :: [a] -> Maybe a
head' [] = Nothing
head' (a : _) = Just a

isMirror :: (Eq a) => [a] -> [a] -> Bool
isMirror x y = all (uncurry (==)) (reverse x `zip` y)

scoreGrid1 :: Grid -> Integer
scoreGrid1 g =
  100 * fromMaybe 0 (findMirrorPoint $ rows g)
    + fromMaybe 0 (findMirrorPoint $ columns g)

isOneOffMirror :: [Integer] -> [Integer] -> Bool
isOneOffMirror x y = 1 == sum (zipWith (\a b -> popCount $ a `xor` b) (reverse x) y)

findSmudgeMirrorPoint :: [Integer] -> Maybe Integer
findSmudgeMirrorPoint l = head' $ map toInteger $ filter (\i -> take i l `isOneOffMirror` drop i l) [1 .. length l - 1]

scoreGrid2 :: Grid -> Integer
scoreGrid2 g =
  100 * fromMaybe 0 (findSmudgeMirrorPoint $ rows g)
    + fromMaybe 0 (findSmudgeMirrorPoint $ columns g)
