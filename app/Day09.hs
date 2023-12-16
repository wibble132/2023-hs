module Day09 (part1, part2) where

-- 1708206096 Correct :)
part1 :: String -> String
part1 = show . sum . map (extrapolate . map (read :: String -> Integer) . words) . lines

-- 1050 Correct :) 
part2 :: String -> String
part2 = show . sum . map (extrapolate . reverse . map (read :: String -> Integer) . words) . lines

type Sequence = [Integer]

extrapolate :: Sequence -> Integer
extrapolate s =
  if all (== 0) s
    then 0
    else last s + extrapolate (diffs s)

diffs :: Sequence -> Sequence
diffs = map (uncurry $ flip (-)) . getPairs

getPairs :: Sequence -> [(Integer, Integer)]
getPairs [] = []
getPairs [_] = []
getPairs (a : b : cs) = (a, b) : getPairs (b : cs)