module Day05 (part1, part2, part2Better) where

import Data.List (foldl', sortBy, stripPrefix)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Data.Maybe (fromJust)

-- 462648396 Correct :)
part1 :: String -> String
part1 = show . minimum . (\x -> applyAllMapsToSeeds x $ seedsList x) . parseInput

-- 2520479 Correct :) (brute force)
part2 :: String -> String
part2 = show . minimum . (\x -> applyAllMapsToSeeds x $ pairedSeedsIntervals $ seedsList x) . parseInput

part2Better :: String -> String
part2Better = unlines . map show . (\x -> applyAllFarmMapsToIntervals x $ pairedSeeds $ seedsList x) . parseInput
-- part2Better = show . soilToFertilizer . parseInput

data FarmMap = FarmMap
  { mapDestStart :: Integer,
    mapSourceStart :: Integer,
    mapLength :: Integer
  }
  deriving (Show)

_mapSourceLast :: FarmMap -> Integer
_mapSourceLast farmMap = mapSourceStart farmMap + mapLength farmMap - 1

mapSourceInterval :: FarmMap -> Interval
mapSourceInterval farmMap = Interval (mapSourceStart farmMap) (mapLength farmMap)

_mapDestInterval :: FarmMap -> Interval
_mapDestInterval farmMap = Interval (mapDestStart farmMap) (mapLength farmMap)

data Almanac = Almanac
  { seedsList :: [Integer],
    seedToSoil :: [FarmMap],
    soilToFertilizer :: [FarmMap],
    fertilizerToWater :: [FarmMap],
    waterToLight :: [FarmMap],
    lightToTemperature :: [FarmMap],
    temperatureToHumidity :: [FarmMap],
    humidityToLocation :: [FarmMap]
  }
  deriving (Show)

parseInput :: String -> Almanac
parseInput = parseInput' . splitOn "\n\n"

parseInput' :: [String] -> Almanac
parseInput' (seeds : maps) = makeAlmanac (parseSeeds seeds) (map (parseFarmMap . lines) maps)
parseInput' _ = error "Bad input"

parseSeeds :: String -> [Integer]
parseSeeds = map read . words . fromJust . stripPrefix "seeds: "

parseFarmMap :: [String] -> [FarmMap]
parseFarmMap (_ : farmMaps) = map (makeFarmMap . map read . words) farmMaps
parseFarmMap _ = error "Bad farm maps"

makeFarmMap :: [Integer] -> FarmMap
makeFarmMap (a : b : c : _) = FarmMap a b c
makeFarmMap _ = error "Bad farm map"

makeAlmanac :: [Integer] -> [[FarmMap]] -> Almanac
makeAlmanac seeds (m1 : m2 : m3 : m4 : m5 : m6 : m7 : _) = Almanac seeds m1 m2 m3 m4 m5 m6 m7
makeAlmanac _ _ = error "Bad almanac"

applyFarmMaps :: [FarmMap] -> Integer -> Integer
applyFarmMaps maps source =
  if null possibleMaps then source else applyFarmMap (head possibleMaps) source
  where
    possibleMaps = filter (\farmMap -> mapSourceStart farmMap <= source && source < mapSourceStart farmMap + mapLength farmMap) maps

applyFarmMap :: FarmMap -> Integer -> Integer
applyFarmMap farmMap source = mapDestStart farmMap + source - mapSourceStart farmMap

applyAllMapsToSeeds :: Almanac -> [Integer] -> [Integer]
applyAllMapsToSeeds a seeds = foldl' (flip map) seeds (map (\f -> applyFarmMaps $ f a) allFarmMaps)

allFarmMaps :: [Almanac -> [FarmMap]]
allFarmMaps = [seedToSoil, soilToFertilizer, fertilizerToWater, waterToLight, lightToTemperature, temperatureToHumidity, humidityToLocation]

pairedSeedsIntervals :: [Integer] -> [Integer]
pairedSeedsIntervals (a : b : rest) = [a .. (a + b - 1)] ++ pairedSeedsIntervals rest
pairedSeedsIntervals [] = []
pairedSeedsIntervals _ = error "Bad seeds input"

data Interval = Interval {intervalStart :: Integer, intervalLength :: Integer} deriving (Show)

overlaps :: Interval -> Interval -> Bool
overlaps i1 i2 = (intervalStart i2 `element` i1) || (intervalStart i1 `element` i2)

element :: Integer -> Interval -> Bool
element p i = intervalStart i <= p && p <= intervalLast i

intervalLast :: Interval -> Integer
intervalLast (Interval a b) = a + b - 1

pairedSeeds :: [Integer] -> [Interval]
pairedSeeds (a : b : rest) = Interval a b : pairedSeeds rest
pairedSeeds [] = []
pairedSeeds _ = error "Bad seeds input"

applyFarmMapsToInterval :: [FarmMap] -> Interval -> [Interval]
applyFarmMapsToInterval farmMaps i = case applicableMaps of
  [] -> [i]
  -- This is wrong ffs
  -- Should consider each of the lowest, middle, highest individually
  -- Either that or fill out the farmMaps to have identity intervals covered (which is probably worse to do)
  [singleMap] -> [Interval (applyFarmMap singleMap $ intervalStart i) (intervalLength i)]
  [leftMap, rightMap] -> [applyFarmMapToInvervalBottom leftMap i, applyFarmMapToInvervalTop rightMap i]
  (leftMap : moreMaps) ->
    applyFarmMapToInvervalBottom leftMap i
      : applyFarmMapToInvervalTop (last moreMaps) i
      : map (\m -> Interval (mapDestStart m) (mapLength m)) (init moreMaps)
  where
    applicableMaps =
      sortBy (comparing mapSourceStart) $
        filter (\x -> mapSourceInterval x `overlaps` i) farmMaps

--- Map      |------|
--- Int         |------|
applyFarmMapToInvervalBottom :: FarmMap -> Interval -> Interval
applyFarmMapToInvervalBottom farmMap i =
  Interval (applyFarmMap farmMap $ intervalStart i) (mapLength farmMap - (intervalStart i - mapSourceStart farmMap))

--- Map         |------|
--- Int      |------|
applyFarmMapToInvervalTop :: FarmMap -> Interval -> Interval
applyFarmMapToInvervalTop farmMap i =
  Interval (mapDestStart farmMap) (intervalLength i - (mapSourceStart farmMap - intervalStart i))

applyFarmMapsToIntervals :: [FarmMap] -> [Interval] -> [Interval]
applyFarmMapsToIntervals farmMaps =
  combineAdjacentIntervals
    . sortBy (comparing (\(Interval a _) -> a))
    . foldl' (\acc interval -> acc ++ applyFarmMapsToInterval farmMaps interval) []

combineAdjacentIntervals :: [Interval] -> [Interval]
combineAdjacentIntervals [] = []
combineAdjacentIntervals [i] = [i]
combineAdjacentIntervals (i1@(Interval a1 _) : i2@(Interval a2 b2) : rest)
  | intervalLast i1 < a2 = i1 : combineAdjacentIntervals (i2 : rest)
  | otherwise = combineAdjacentIntervals (Interval a1 (a2 + b2 - a1) : rest)

applyAllFarmMapsToIntervals :: Almanac -> [Interval] -> [Interval]
applyAllFarmMapsToIntervals a = applyFarmMapsToIntervals (soilToFertilizer a) . applyFarmMapsToIntervals (seedToSoil a)
