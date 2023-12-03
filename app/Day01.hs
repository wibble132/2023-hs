module Day01 where

import Data.Char (digitToInt, isDigit)

count :: (Foldable t) => t a -> [Char]
count s = show (length s) ++ "\n"

-- 54609 too low - missed single digits (e.g. "egg6cheese" should give 66)
-- 56042 correct
part1 :: String -> String
part1 n = show (sum (map getCalibrationValue (lines n)))

getCalibrationValue :: String -> Int
getCalibrationValue s = pairToNum (firstAndLast (map digitToInt (filter isDigit s)))

firstAndLast :: [Int] -> (Int, Int)
firstAndLast [] = error "Empty list given"
firstAndLast [x] = (x, x)
firstAndLast (x : y) = (x, last y)

pairToNum :: (Int, Int) -> Int
pairToNum (a, b) = 10 * a + b

-- 55362 too high - Didn't consider "eightwo" as counting as 8 and 2
-- 55358 correct
part2 :: String -> String
part2 n = show (sum (map getCalibrationValue2 (lines n)))

getCalibrationValue2 :: String -> Int
getCalibrationValue2 s = getCalibrationValue (replaceSpeltDigits s)

replaceSpeltDigits :: String -> String
replaceSpeltDigits = speltToDigit . replaceSpeltDigits'

-- can be tidied?
replaceSpeltDigits' :: String -> String
replaceSpeltDigits' ('o' : 'n' : 'e' : p) = '1' : replaceSpeltDigits ("e" ++ p)
replaceSpeltDigits' ('t' : 'w' : 'o' : p) = '2' : replaceSpeltDigits ("wo" ++ p)
replaceSpeltDigits' ('t' : 'h' : 'r' : 'e' : 'e' : p) = '3' : replaceSpeltDigits ("hree" ++ p)
replaceSpeltDigits' ('f' : 'o' : 'u' : 'r' : p) = '4' : replaceSpeltDigits ("our" ++ p)
replaceSpeltDigits' ('f' : 'i' : 'v' : 'e' : p) = '5' : replaceSpeltDigits ("ive" ++ p)
replaceSpeltDigits' ('s' : 'i' : 'x' : p) = '6' : replaceSpeltDigits ("ix" ++ p)
replaceSpeltDigits' ('s' : 'e' : 'v' : 'e' : 'n' : p) = '7' : replaceSpeltDigits ("even" ++ p)
replaceSpeltDigits' ('e' : 'i' : 'g' : 'h' : 't' : p) = '8' : replaceSpeltDigits ("ight" ++ p)
replaceSpeltDigits' ('n' : 'i' : 'n' : 'e' : p) = '9' : replaceSpeltDigits ("ine" ++ p)
replaceSpeltDigits' (x : y) = x : replaceSpeltDigits y
replaceSpeltDigits' x = x

speltToDigit :: String -> String
speltToDigit "one" = "1"
speltToDigit "two" = "2"
speltToDigit "three" = "3"
speltToDigit "four" = "4"
speltToDigit "five" = "5"
speltToDigit "six" = "6"
speltToDigit "seven" = "7"
speltToDigit "eight" = "8"
speltToDigit "nine" = "9"
speltToDigit s = s