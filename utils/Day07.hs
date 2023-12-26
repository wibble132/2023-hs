{-# LANGUAGE InstanceSigs #-}

module Day07 (part1, part2) where

import Data.Bifunctor (Bifunctor (first))
import Data.List (sortOn)
import Utils (mapOneIndexed)

-- 249483956 Correct :)
part1 :: String -> String
part1 = show . sum . mapOneIndexed (*) . map snd . sortOn fst . value . parseInput
-- 252137472 Correct :)
part2 :: String -> String
part2 = show . sum . mapOneIndexed (*) . map snd . sortOn fst . value . mapToPart2 . parseInput

data Card = Joker | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | T | J | Q | K | A deriving (Show, Eq, Ord)

data Hand = Hand [Card] HandScore deriving (Show, Eq)

data HandScore = HighCard | Pair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show, Eq, Ord)

newtype Input = Input {value :: [(Hand, Integer)]}

instance Show Input where
  show (Input l) = unlines $ map show l

instance Ord Hand where
  (<=) :: Hand -> Hand -> Bool
  (<=) (Hand h1 s1) (Hand h2 s2)
    | s1 /= s2 = s1 <= s2
    | otherwise = h1 <= h2

instance Read Card where
  readsPrec :: Int -> ReadS Card
  readsPrec _ s = case s of
    ('A' : ss) -> [(A, ss)]
    ('K' : ss) -> [(K, ss)]
    ('Q' : ss) -> [(Q, ss)]
    ('J' : ss) -> [(J, ss)]
    ('T' : ss) -> [(T, ss)]
    ('9' : ss) -> [(N9, ss)]
    ('8' : ss) -> [(N8, ss)]
    ('7' : ss) -> [(N7, ss)]
    ('6' : ss) -> [(N6, ss)]
    ('5' : ss) -> [(N5, ss)]
    ('4' : ss) -> [(N4, ss)]
    ('3' : ss) -> [(N3, ss)]
    ('2' : ss) -> [(N2, ss)]
    _ -> error "Bad read"

parseInput :: String -> Input
parseInput = Input . map (parseInputLine . words) . lines

parseInputLine :: [String] -> (Hand, Integer)
parseInputLine [cards, bet] = (parseCards cards, read bet)
parseInputLine _ = error "Bad input line"

parseCards :: String -> Hand
parseCards s = Hand cards (getScore cards)
  where
    cards = map (read . (: [])) s

allCards :: [Card]
allCards = [A, K, Q, J, T, N9, N8, N7, N6, N5, N4, N3, N2]

getScore :: [Card] -> HandScore
getScore cards = case counts of
  (5 : _) -> FiveOfAKind
  (4 : _) -> FourOfAKind
  (3 : 2 : _) -> FullHouse
  (3 : _) -> ThreeOfAKind
  (2 : 2 : _) -> TwoPair
  (2 : _) -> Pair
  _ -> HighCard
  where
    counts = sortOn (\x -> -x) $ map (\card -> length $ filter (== card) cards) allCards

mapToPart2 :: Input -> Input
mapToPart2 (Input l) = Input $ map (first mapToPart2') l

jackToJoker :: Card -> Card
jackToJoker x = case x of
  J -> Joker
  _ -> x

mapToPart2' :: Hand -> Hand
mapToPart2' (Hand cards _) =
  Hand cards' $ getScorePart2 cards'
  where
    cards' = map jackToJoker cards

getScorePart2 :: [Card] -> HandScore
getScorePart2 = maximum . map getScore . iterateJokerPossibilities

iterateJokerPossibilities :: [Card] -> [[Card]]
iterateJokerPossibilities [] = []
iterateJokerPossibilities [Joker] = map (: []) allCards
iterateJokerPossibilities [c] = [[c]]
iterateJokerPossibilities (c : cs)
  | c == Joker = concatMap (\c' -> map ([c'] ++) $ iterateJokerPossibilities cs) allCards
  | otherwise = map ([c] ++) $ iterateJokerPossibilities cs
