{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Day12 (part1, part2) where

import Control.Monad.Fix (fix)
import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.List (group, sort)
import Data.List.Split (splitOn)
import Data.Map (Map, empty, (!))
import Data.MemoTrie (HasTrie (..), Reg, enumerateGeneric, memo, memo2, memoFix, trieGeneric, untrieGeneric)
import GHC.Generics (Generic)

-- 8419 Correct :)
part1 :: String -> String
part1 = show . sum . map (countArrangements2 . parseLine) . lines

-- 30 - 30s
-- 40 - 30s
-- only line 42 - 15 minutes -- this isn't gonna work...
-- rethink time ;)
-- 160500973317706 first memo "working" - too high (but only 7s realtime :) )
-- 5604841818 too low
-- 160500973317706 correct 
part2 :: String -> String
part2 = show . sum . map p2. lines

-- 5604841818
-- 5604841818

-- part2 = show . countArrangements . parseLine . mapLinePart2 5 . const "????#????.??? 1,2,1,1"

-- part2 = show . parseSprings2 [1, 2] . const "????#????.????????#????.????????#????.????????#????.????????#????.???"

p2 :: String -> Integer
p2 = countArrangements2 . parseLine . mapLinePart2 5

func2 as = parMap rdeepseq p2

data Spring = Unknown | Damaged | Operational deriving (Eq, Ord, Generic)

data Input = Input [Spring] [Int] deriving (Show)

instance Read Spring where
  readsPrec :: Int -> ReadS Spring
  readsPrec _ ('?' : s) = [(Unknown, s)]
  readsPrec _ ('#' : s) = [(Damaged, s)]
  readsPrec _ ('.' : s) = [(Operational, s)]
  readsPrec _ _ = []

instance Show Spring where
  show :: Spring -> String
  show Unknown = "?"
  show Damaged = "#"
  show Operational = "."

parseLine :: String -> Input
parseLine = parseParts . words
  where
    parseParts [a, b] = Input (map (read . (: [])) a) (map read $ splitOn "," b)
    parseParts _ = Input [] []

type Args = (Bool, [Spring], [Integer])

countArrangements :: Input -> Integer
countArrangements (Input springs lengths) = count False springs lengths
  where
    count :: Bool -> [Spring] -> [Int] -> Integer
    count True (s : ss) (l : ls)
      | s == Damaged && l > 0 = count True ss (l - 1 : ls)
      | s == Damaged && l == 0 = 0
      | s == Operational && l > 0 = 0
      | s == Operational && l == 0 = count False ss ls
      | s == Unknown && l > 0 = count True ss (l - 1 : ls)
      | s == Unknown && l == 0 = count False ss ls
      | l < 0 = error "bad"
    count False (s : ss) la@(l : ls)
      | s == Damaged && l > 0 = count True ss (l - 1 : ls)
      | s == Damaged && l == 0 = 0
      | s == Operational && l > 0 = count False ss la
      | s == Operational && l == 0 = count False ss ls
      | s == Unknown && l > 0 = count True ss (l - 1 : ls) + count False ss la
      | s == Unknown && l == 0 = count False ss ls
      | l < 0 = error "bad"
    count _ sa [] = if Damaged `elem` sa then 0 else 1
    count _ [] [0] = 1
    count _ [] _ = 0
    count a b c = error $ "Missing pattern?" ++ show (a, b, c)

repeatWithSeparator :: Char -> Int -> String -> String
repeatWithSeparator sep count s = (++ s) $ concat $ replicate (count - 1) (s ++ [sep])

mapLinePart2 :: Int -> String -> String
mapLinePart2 n = unwords . map (\(c, s) -> repeatWithSeparator c n s) . (['?', ','] `zip`) . words

countArrangements2 :: Input -> Integer
countArrangements2 (Input springs lengths) = memoCount springs lengths
  where
    memoCount = fix (memo2 . count)
    count :: ([Spring] -> [Int] -> Integer) -> [Spring] -> [Int] -> Integer

    -- Want no more springs, can we do that?
    count _ s [] = if Damaged `notElem` s then 1 else 0

    -- move past a block of operational springs
    count f (Operational:s) l = f s l
    
    -- No springs left, return 0
    count _ [] _ = 0

    count f curr_vals@(s : ss) nums_to_fit@(l : ls)
      | canTake && canSkip = callTake + callSkip
      | canTake = callTake
      | canSkip = callSkip
      | otherwise = 0
      where
        -- Are the next l '?' or '#', followed by '#', '?' or nothing
        canTake = length curr_vals >= l 
                  && Operational `notElem` take l curr_vals 
                  && (length curr_vals == l || Damaged /= curr_vals !! l)
        -- Is the next spring '.' or '?'
        canSkip = s /= Damaged
        -- Case with 
        callTake = f (drop (l + 1) curr_vals) ls
        callSkip = f ss nums_to_fit

instance HasTrie Spring where
  newtype Spring :->: b = SpringTrie {unSpringTrie :: Reg Spring :->: b}
  trie = trieGeneric SpringTrie
  untrie = untrieGeneric unSpringTrie
  enumerate = enumerateGeneric unSpringTrie