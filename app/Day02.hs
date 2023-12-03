{-# LANGUAGE InstanceSigs #-}

module Day02 where

import Data.List.Split (splitOn)

data Colour = Red | Green | Blue deriving (Show, Eq)

-- Custom implmentation to read lowercase values
instance Read Colour where
  readsPrec :: Int -> ReadS Colour
  readsPrec _ "red" = [(Red, "")]
  readsPrec _ "green" = [(Green, "")]
  readsPrec _ "blue" = [(Blue, "")]
  readsPrec _ _ = []

type Count = Integer

data Line = Line Int [Pull] deriving (Show)

newtype Pull = Pull [(Count, Colour)] deriving (Show)

-- Each line is of the form
-- "Game {id}: [[{count} {colour}, ...]; ...]"
readLine :: String -> Line
readLine = parseLine . words

parseLine :: [String] -> Line
parseLine ("Game" : lineId : pulls) = Line (read (init lineId) :: Int) (parsePulls (unwords pulls))
parseLine _ = error "Bad line in input"

parsePulls :: String -> [Pull]
parsePulls s = map (parsePull . splitOn ", ") (splitOn "; " s)

-- Takes "5 Red, 4 Blue, 2 Green" to Pull [(5,"Red"),(4,"Blue"),...]
parsePull :: [String] -> Pull
parsePull s = Pull (map parseCountColour s)

parseCountColour :: String -> (Count, Colour)
parseCountColour s = case splitOn " " s of
  (n : col : _) -> (read n, read col)
  other -> error ("Bad count colour: " ++ show other)

baselinePull :: Pull
baselinePull = Pull [(12, Red), (13, Green), (14, Blue)]

-- 2447 correct :)
part1 :: String -> String
part1 = show . sum . map getId . filter (pullsLessThan baselinePull) . map readLine . lines

pullsLessThan :: Pull -> Line -> Bool
pullsLessThan baseline (Line _ testPulls) = all (pullLessThan baseline) testPulls

pullLessThan :: Pull -> Pull -> Bool
pullLessThan (Pull baseline) (Pull testPull) = all (\(count, colour) -> getCountForColour colour (Pull testPull) <= count) baseline

getCountForColour :: Colour -> Pull -> Count
getCountForColour testCol (Pull ((count, col) : ccs)) = if testCol == col then count else getCountForColour testCol (Pull ccs)
getCountForColour _ (Pull []) = 0

getId :: Line -> Int
getId (Line lineId _) = lineId

-- 56322 Correct :)
part2 :: String -> String
part2 = show . sum . map (power . readLine) . lines 

colours :: [Colour]
colours = [Red, Green, Blue]

power :: Line -> Count
power (Line _ pulls) = (product . map fst . getMinTotal) pulls

getMinTotal :: [Pull] -> [(Count, Colour)]
getMinTotal pulls = map (\col -> (maximum ( map (getCountForColour col) pulls), col)) colours