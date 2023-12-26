{-# LANGUAGE GADTs #-}

module Day15 (part1, part2) where

import Data.Char (digitToInt, ord)
import Data.Foldable (foldl')
import Data.List.Split (splitOn)
import GHC.Arr (Array, array, elems, (!), (//))
import Utils (mapOneIndexed)

-- 511215 Correct :)
part1 :: String -> String
part1 = show . sum . map (toInteger . hash) . splitOn ","

-- 236057 Correct :)
part2 :: String -> String
part2 = show . focusingPower . foldl' applyInstruction (array (0, 255) [(i, []) | i <- [0 .. 255]]) . parseInput

hash :: String -> Int
hash = foldl' (\curr c -> (17 * (curr + ord c)) `mod` 256) 0

data Lens = Lens {label :: String, focalLength :: Int} deriving (Show)

data Instruction = Add String Int | Remove String deriving (Show)

parseInput :: String -> [Instruction]
parseInput = map parseInstr . splitOn ","
  where
    parseInstr :: String -> Instruction
    parseInstr s
      | '-' `elem` s = Remove $ init s
      | '=' `elem` s = Add (init $ init s) (digitToInt $ last s)
      | otherwise = error $ "Bad instruction: " ++ s

applyInstruction :: Array Int [Lens] -> Instruction -> Array Int [Lens]
applyInstruction a (Add lab pow)
  | any (\x -> label x == lab) $ a ! hash lab = a // [(hash lab, map (\x -> if label x /= lab then x else x {focalLength = pow}) (a ! hash lab))]
  | otherwise = a // [(hash lab, (a ! hash lab) ++ [Lens lab pow])]
applyInstruction a (Remove lab) = a // [(hash lab, filter (\x -> label x /= lab) (a ! hash lab))]

focusingPower :: Array Int [Lens] -> Integer
focusingPower = sum . mapOneIndexed (\i x -> sum $ mapOneIndexed (\j l -> j * i * toInteger (focalLength l)) x) . elems