module Main where

import Day01 (part1, part2)
import Day02 (part1, part2)
import Day03 (part1, part2)
import Day04 (part1, part2)
import Day05 (part1, part2, part2Better)
import Day07 (part1, part2)
import Day08 (part1, part2)
import Day09 (part1, part2)
import Day10 (part1, part2)
import Day11 (part1, part2)

main :: IO ()
main = do
  putStrLn "Day 1"
  day1 <- getInput 1
  putStrLn (Day01.part1 day1)
  putStrLn (Day01.part2 day1)

  putStrLn "Day 2"
  -- day2 <- getExample 2 1
  day2 <- getInput 2
  putStrLn (Day02.part1 day2)
  putStrLn (Day02.part2 day2)

  putStrLn "Day 3"
  -- day3 <- getExample 3 1
  day3 <- getInput 3
  putStrLn (Day03.part1 day3)
  putStrLn (Day03.part2 day3)

  putStrLn "Day 4"
  -- day4 <- getExample 4 1
  day4 <- getInput 4
  putStrLn (Day04.part1 day4)
  putStrLn (Day04.part2 day4)

  putStrLn "Day 5"
  day5 <- getExample 5 1
  -- day5 <- getInput 5
  putStrLn (Day05.part1 day5)
  putStrLn (Day05.part2 day5)
  putStrLn (Day05.part2Better day5)

  putStrLn "Day 7"
  -- day7 <- getExample 7 1
  day7 <- getInput 7
  putStrLn (Day07.part1 day7)
  putStrLn (Day07.part2 day7)

  putStrLn "Day 8"
  -- day8 <- getExample 8 3
  day8 <- getInput 8
  -- day8_2 <- getExample 8 3
  day8_2 <- getInput 8
  putStrLn (Day08.part1 day8)
  putStrLn (Day08.part2 day8_2)

  putStrLn "Day 9"
  -- day9 <- getExample 9 1
  day9 <- getInput 9
  putStrLn (Day09.part1 day9)
  putStrLn (Day09.part2 day9)

  putStrLn "Day 10"
  -- day10 <- getExample 10 4
  day10 <- getInput 10
  putStrLn (Day10.part1 day10)
  putStrLn (Day10.part2 day10)

  putStrLn "Day 11"
  -- day11 <- getExample 11 1
  day11 <- getInput 11
  putStrLn (Day11.part1 day11)
  putStrLn (Day11.part2 day11)

getInput :: Int -> IO String
getInput day = readFile ("app/data/day" ++ show day ++ ".txt")

getExample :: Int -> Int -> IO String
getExample day exNum = readFile ("app/data/day" ++ show day ++ "-e" ++ show exNum ++ ".txt")