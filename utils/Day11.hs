{-# LANGUAGE TupleSections #-}

module Day11 (part1, part2) where

import Data.Bifunctor (first)
import Data.Tuple (swap)
import Utils (withIndex, mapIndexed)

-- 9742154 Correct :)
part1 :: String -> String
part1 = show . sum . map (uncurry dist) . getAllPairs . expandAll . parseInput

part2 :: String -> String
part2 = show . sum . map (uncurry dist) . getAllPairs . expandAllBy 999999 . parseInput

parseInput :: String -> [Pos]
parseInput = concat . mapIndexed parseLine . lines

parseLine :: Integer -> String -> [Pos]
parseLine i = map (\(j, _) -> (i, j)) . filter ((/= '.') . snd) . withIndex

type Pos = (Integer, Integer)

expandAll :: [Pos] -> [Pos]
expandAll = map swap . expandColumns . map swap . expandColumns

expandAllBy :: Integer -> [Pos] -> [Pos]
expandAllBy t = map swap . expandColumnsBy t . map swap . expandColumnsBy t

expandColumnsBy :: Integer -> [Pos] -> [Pos]
expandColumnsBy gap ps = step2 $ snd $ foldl step1 (0, map (,0) ps) [0 .. maximum $ map fst ps]
  where
    step1 :: (Integer, [(Pos, Integer)]) -> Integer -> (Integer, [(Pos, Integer)])
    step1 (offset, p) col
      | null posInCol = (offset + gap, p)
      | otherwise = (offset, map (\a@(b@(x, _), _) -> if x == col then (b, offset) else a) p)
      where
        posInCol = filter ((== col) . fst . fst) p

    step2 :: [(Pos, Integer)] -> [Pos]
    step2 = map (\(p, offset) -> first (+ offset) p)

expandColumns :: [Pos] -> [Pos]
expandColumns = expandColumnsBy 1

getAllPairs :: [a] -> [(a,a)]
getAllPairs (x:xs) = map (x,) xs ++ getAllPairs xs
getAllPairs [] = []

dist :: Pos -> Pos -> Integer
dist (a,b) (c,d) = abs (a-c) + abs (b-d)