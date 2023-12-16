module Day04 (part1, part2) where

import Data.List.Split (splitOn)

-- 20107 Correct :)
part1 :: String -> String
part1 = show . sum . map (getPoints . getMatches) . parseInput

-- 8172507 Correct :)
part2 :: String -> String
part2 = show . countTickets . map getMatches . parseInput

data Ticket = Ticket
  { cardNumber :: Int,
    winningNumbers :: [Int],
    chosenNumbers :: [Int]
  }
  deriving (Show)

parseInput :: String -> [Ticket]
parseInput = map (parseLine . filter (/= []) . words) . lines

parseLine :: [String] -> Ticket
parseLine ("Card" : n : rest) = parseTicketNumbers (read n) (splitOn " | " $ unwords rest)
parseLine _ = error "Bad line"

parseTicketNumbers :: Int -> [String] -> Ticket
parseTicketNumbers n (winning : chosen : _) = Ticket n (map read $ words winning) (map read $ words chosen)
parseTicketNumbers _ _ = error "Bad ticket numbers"

getMatches :: Ticket -> Int
getMatches (Ticket _ winning chosen) = length $ filter (`elem` winning) chosen

getPoints :: Int -> Integer
getPoints 0 = 0
getPoints n = 2 ^ (n - 1)

-- have a list of ints
-- result[0] = 1
-- result[i] = 1 + sum_k=0^{i-1} (result[k] * (in[k] >= i-k))
countTickets :: [Int] -> Int
countTickets input = sum $ foldl (\soFarResult _ -> countTickets' soFarResult input) [] [0 .. length input - 1]

countTickets' :: [Int] -> [Int] -> [Int]
countTickets' soFarResult input = soFarResult ++ [1 + sum (zipWith (\(resk, ink) k -> if i - k <= ink then resk else 0) (soFarResult `zip` input) [0 .. i - 1])]
  where i = length soFarResult