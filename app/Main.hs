module Main where

import Day01 (part1, part2)
import Day02 (part1, part2)
import Day03 (part1, part2)
import Day04 (part1, part2)

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


getInput :: Int -> IO String
getInput day = readFile ("app/data/day" ++ show day ++ ".txt")

getExample :: Int -> Int -> IO String
getExample day exNum = readFile ("app/data/day" ++ show day ++ "-e" ++ show exNum ++ ".txt")