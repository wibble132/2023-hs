module Day03 where

import Data.Char (digitToInt, isDigit)

-- 536576 Correct :)
part1 :: String -> String
part1 = show . sumPartNumbers . parseInput

-- 75741499 Correct :)
part2 :: String -> String
part2 s = show $ sum $ map (gearRatio schem) $ symbols schem
  where
    schem = parseInput s

data PartNumber = PartNumber
  { num :: Integer,
    partLineNumber :: Int,
    start :: Int,
    end :: Int
  }
  deriving (Show)

data Schematic = Schematic {numbers :: [PartNumber], symbols :: [Symbol]} deriving (Show)

data Symbol = Symbol {val :: Char, symbolLineNum :: Int, symbolLinePos :: Int} deriving (Show)

parseInput :: String -> Schematic
parseInput = foldlOneIndexed (\i schem line -> parseLine line i 1 schem) (Schematic [] []) . lines

mapOneIndexed :: (Int -> a -> b) -> [a] -> [b]
mapOneIndexed f = map (uncurry f) . withOneIndex

foldlOneIndexed :: (Int -> b -> a -> b) -> b -> [a] -> b
foldlOneIndexed f b l = foldl (\b1 (i, a) -> f i b1 a) b (withOneIndex l)

withOneIndex :: [a] -> [(Int, a)]
withOneIndex l = [1 .. length l] `zip` l

parseLine :: String -> Int -> Int -> Schematic -> Schematic
parseLine (c : cs) lineNum linePos schem
  | c == '.' = parseLine cs lineNum (linePos + 1) schem
  | isDigit c = parseLine cs lineNum (linePos + 1) (addDigit ((toInteger . digitToInt) c) lineNum linePos schem)
  | otherwise = parseLine cs lineNum (linePos + 1) (addSymbol c lineNum linePos schem)
parseLine "" _ _ schem = schem

addDigit :: Integer -> Int -> Int -> Schematic -> Schematic
addDigit digit lineNum linePos schem = case (length . numbers) schem of
  0 -> schem {numbers = [PartNumber digit lineNum linePos linePos]}
  _ ->
    if (end . head . numbers) schem == linePos - 1
      then schem {numbers = ((head . numbers) schem) {num = (num . head . numbers) schem * 10 + digit, end = linePos} : (tail . numbers) schem}
      else schem {numbers = PartNumber digit lineNum linePos linePos : numbers schem}

addSymbol :: Char -> Int -> Int -> Schematic -> Schematic
addSymbol symbol lineNum linePos schem = schem {symbols = Symbol symbol lineNum linePos : symbols schem}

getNumbersSurrounding :: Symbol -> Schematic -> [PartNumber]
getNumbersSurrounding symbol = filter (`doesSurround` symbol) . numbers

doesSurround :: PartNumber -> Symbol -> Bool
doesSurround number sym =
  (x1 - 1 <= x2 && x2 <= x1' + 1) && (l1 - 1 <= l2 && l2 <= l1 + 1)
  where
    x1 = start number
    x1' = end number
    l1 = partLineNumber number
    x2 = symbolLinePos sym
    l2 = symbolLineNum sym

sumPartNumbers :: Schematic -> Integer
sumPartNumbers schem =
  sum $ map num $ filter (surroundsSymbol schem) $ numbers schem

surroundsSymbol :: Schematic -> PartNumber -> Bool
surroundsSymbol schem partNumber = any (partNumber `doesSurround`) $ symbols schem

gearRatio :: Schematic -> Symbol -> Integer
gearRatio schem symbol@(Symbol '*' _ _) =
  case filter (`doesSurround` symbol) (numbers schem) of
    [a, b] -> num a * num b
    _ -> 0
gearRatio _ _ = 0