module Day01 (part1, part2) where

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)

-- 54609 too low - missed single digits (e.g. "egg6cheese" should give 66)
-- 56042 correct
part1 :: String -> String
part1 = show . sum . map getCalibrationValue . lines

-- 55362 too high - Didn't consider "eightwo" as counting as 8 and 2
-- 55358 correct
part2 :: String -> String
part2 = show . sum . map (getCalibrationValue . replaceSpeltDigits) . lines

getCalibrationValue :: String -> Int
getCalibrationValue = pairToNum . firstAndLast . map digitToInt . filter isDigit

firstAndLast :: [Int] -> (Int, Int)
firstAndLast [] = error "Empty list given"
firstAndLast [x] = (x, x)
firstAndLast (x : y) = (x, last y)

pairToNum :: (Int, Int) -> Int
pairToNum (a, b) = 10 * a + b

-- swap the first character of spelt digits with that digit
replaceSpeltDigits :: String -> String
replaceSpeltDigits [] = []
replaceSpeltDigits s = c : rest
  where
    t = map snd $ filter (\(num, _) -> num `isPrefixOf` s) speltDigits
    c = if null t then head s else head t
    rest = replaceSpeltDigits (tail s)

speltDigits :: [(String, Char)]
speltDigits =
  [ ("one", '1'),
    ("two", '2'),
    ("three", '3'),
    ("four", '4'),
    ("five", '5'),
    ("six", '6'),
    ("seven", '7'),
    ("eight", '8'),
    ("nine", '9')
  ]