module Main where

import Day01 (part1, part2)
import Day02 (part1, part2)
import Day03 (part1, part2)
import Day04 (part1, part2)
import Day05 (part1, part2) -- , part2Better)
import Day07 (part1, part2)
import Day08 (part1, part2)
import Day09 (part1, part2)
import Day10 (part1, part2)
import Day11 (part1, part2)
import Day12 (part1, part2)

main :: IO ()
main = day 12 1

day :: Int -> Int -> IO ()
day d i = do
  putStrLn $ "Day " ++ show d
  input <- if i == 0 then getInput d else getExample d i
  putStrLn $ getDayPart d 1 input
  putStrLn $ getDayPart d 2 input

getDayPart :: Int -> Int -> (String -> String)
getDayPart d p = case (d,p) of
  (1,1) -> Day01.part1
  (1,2) -> Day01.part2
  (2,1) -> Day02.part1
  (2,2) -> Day02.part2
  (3,1) -> Day03.part1
  (3,2) -> Day03.part2
  (4,1) -> Day04.part1
  (4,2) -> Day04.part2
  (5,1) -> Day05.part1
  (5,2) -> Day05.part2
  (6,_) -> error "Day 6 done by hand"
  (7,1) -> Day07.part1
  (7,2) -> Day07.part2
  (8,1) -> Day08.part1
  (8,2) -> Day08.part2
  (9,1) -> Day09.part1
  (9,2) -> Day09.part2
  (10,1) -> Day10.part1
  (10,2) -> Day10.part2
  (11,1) -> Day11.part1
  (11,2) -> Day11.part2
  (12,1) -> Day12.part1
  (12,2) -> Day12.part2
  _ -> error "Unknown day part"

getInput :: Int -> IO String
getInput d = readFile ("app/data/day" ++ show d ++ ".txt")

getExample :: Int -> Int -> IO String
getExample d exNum = readFile ("app/data/day" ++ show d ++ "-e" ++ show exNum ++ ".txt")