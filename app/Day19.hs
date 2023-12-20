{-# LANGUAGE InstanceSigs #-}

module Day19 (part1, part2) where

import Control.Arrow ((***))
import Control.Monad (join)
import Data.Bifunctor (second)
import Data.Foldable (find)
import Data.List.Split (splitOn, splitOneOf)
import Data.Maybe (fromMaybe, isJust, isNothing)
import GHC.List (foldl')
import Prelude hiding (GT, LT)
import Data.List (partition)

-- 383682 Correct :)
part1 :: String -> String
part1 = show . sum . map addComponents . (\(works, parts) -> filter (passesWorkflows works) parts) . parseInput

-- 117954800808317 Correct :)
part2 :: String -> String
part2 = show . sum . map pointCount . applyWorkflowsUntilSuccess [(defaultRanges, "in")] . fst . parseInput

data Workflow = Workflow {name :: String, rules :: [Rule], def :: String} deriving (Show)

data Comparer = LT | GT deriving (Show, Eq)

data Property = X | M | A | S deriving (Show)

data Rule = Rule {property :: Property, comparer :: Comparer, bound :: Int, dest :: String} deriving (Show)

data Part = Part {x :: Int, m :: Int, a :: Int, s :: Int} deriving (Show)

parseInput :: String -> ([Workflow], [Part])
parseInput = go . splitOn "\n\n"
  where
    go :: [String] -> ([Workflow], [Part])
    go [wrkflws, pts] = (map parseWorkflow $ lines wrkflws, map parsePart $ lines pts)
    go _ = error "Invalid input"

parseWorkflow :: String -> Workflow
parseWorkflow str = Workflow workflowName ruleList defaultRule
  where
    (workflowName, postName) = takeTwo $ splitOneOf "{" str
    ruleStrings = splitOn "," $ init postName
    ruleList = map parseRule $ init ruleStrings
    defaultRule = last ruleStrings

takeTwo :: [b] -> (b, b)
takeTwo [b, c] = (b, c)
takeTwo _ = error "Bad two"

parseRule :: String -> Rule
parseRule _s = Rule prop cond value destStr
  where
    prop = case head _s of
      'x' -> X
      'm' -> M
      'a' -> A
      's' -> S
      _ -> error "Bad property"

    cond = case _s !! 1 of
      '>' -> GT
      '<' -> LT
      _ -> error "Bad condition"

    (valueStr, destStr) = takeTwo $ splitOneOf ":" $ drop 2 _s
    value = read valueStr

parsePart :: String -> Part
parsePart = arrToPart . map (read . drop 2) . splitOn "," . tail . init
  where
    arrToPart :: [Int] -> Part
    arrToPart [_x, _m, _a, _s] = Part _x _m _a _s
    arrToPart _ = error "Invalid part"

passesWorkflows :: [Workflow] -> Part -> Bool
passesWorkflows works p = go $ fromName works "in"
  where
    go :: Workflow -> Bool
    go w = case mapPart w p of
      "A" -> True
      "R" -> False
      n -> go $ fromName works n

fromName :: [Workflow] -> String -> Workflow
fromName works n = case find (\w -> name w == n) works of
  Just _n -> _n
  Nothing -> error $ "Can't find workflow with name: " ++ n

mapPart :: Workflow -> Part -> String
mapPart work p = maybe (def work) dest $ find (p `passes`) $ rules work

passes :: Part -> Rule -> Bool
passes p r = comp prop (bound r)
  where
    prop = case property r of
      X -> x p
      M -> m p
      A -> a p
      S -> s p

    comp = case comparer r of
      GT -> (>)
      LT -> (<)

addComponents :: Part -> Int
addComponents (Part _x _m _a _s) = _x + _m + _a + _s

-- Part 2 looks annoying...

data Range = Range Int Int

instance Show Range where
  show :: Range -> String
  show (Range x1 x2) = "[" ++ show x1 ++ "," ++ show x2 ++ "]"

data Ranges = Ranges {xs :: Range, ms :: Range, as :: Range, ss :: Range} deriving (Show)

defaultRanges :: Ranges
defaultRanges = Ranges (Range 1 4000) (Range 1 4000) (Range 1 4000) (Range 1 4000)

getRange :: Property -> Ranges -> Range
getRange X = xs
getRange M = ms
getRange A = as
getRange S = ss

applyWorkflow :: Ranges -> Workflow -> [(Ranges, String)]
applyWorkflow rs (Workflow _ rls defaultRule) =
  map (second (fromMaybe defaultRule)) $
    foldl' (\acc rule -> filter (isJust . snd) acc ++ concatMap ((`applyRule` rule) . fst) (filter (isNothing . snd) acc)) [(rs, Nothing)] rls

applyRule :: Ranges -> Rule -> [(Ranges, Maybe String)]
applyRule rs' (Rule prop GT b des)
  | x2 <= b = [(rs', Nothing)]
  | x1 <= b = [(rslow, Nothing), (rshigh, Just des)] -- split case
  | otherwise = [(rs', Just des)]
  where
    r@(Range x1 x2) = getRange prop rs'
    (rslow, rshigh) = join (***) (swapRange prop rs') $ splitRange r b (b + 1)
applyRule rs' (Rule prop LT b des)
  | b <= x1 = [(rs', Nothing)]
  | b <= x2 = [(rslow, Just des), (rshigh, Nothing)]
  | otherwise = [(rs', Just des)]
  where
    r@(Range x1 x2) = getRange prop rs'
    (rslow, rshigh) = join (***) (swapRange prop rs') $ splitRange r (b - 1) b

splitRange :: Range -> Int -> Int -> (Range, Range)
splitRange (Range x1 x2) x1' x2' = (Range x1 x1', Range x2' x2)

swapRange :: Property -> Ranges -> Range -> Ranges
swapRange X rs r = rs {xs = r}
swapRange M rs r = rs {ms = r}
swapRange A rs r = rs {as = r}
swapRange S rs r = rs {ss = r}

applyWorkflowsUntilSuccess :: [(Ranges, String)] -> [Workflow] -> [Ranges]
applyWorkflowsUntilSuccess [] _ = []
applyWorkflowsUntilSuccess ((_,"R"):is) wfs = applyWorkflowsUntilSuccess is wfs
applyWorkflowsUntilSuccess ((rs, _name):is) wfs = map fst finished ++ applyWorkflowsUntilSuccess (is ++ continue) wfs
  where
    res = applyWorkflow rs $ fromName wfs _name
    (finished, continue) = partition ((=="A") . snd) res

pointCount :: Ranges -> Integer
pointCount (Ranges _xs _ms _as _ss) = len _xs * len _ms * len _as * len _ss

len :: Range -> Integer
len (Range x1 x2) = toInteger x2 - toInteger x1 + 1