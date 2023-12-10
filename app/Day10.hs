module Day10 (part1, part2) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (Foldable (foldl'))

-- 6754 Correct :)
part1 :: String -> String
part1 s = show $ findFurthestFromSource board
  where
    board = parseInput $ lines s

-- 567 Correct :)
part2 :: String -> String
part2 = show . getInnerArea . onlyMainLoop . parseInput . lines

data Board = Board {pipes :: [Pipe], width :: Int, height :: Int} deriving (Show)

data Pipe = Pipe {position :: Int, exits :: (Int, Int)} | Source {position :: Int} deriving (Show)

instance Eq Pipe where
  p1 == p2 = position p1 == position p2

parseInput :: [[Char]] -> Board
parseInput cs = Board (map (toPipe w) . filter (\(_, c) -> c /= '.') $ withIndex $ concat cs) w (length cs)
  where
    w = length $ head cs

toPipe :: Int -> (Int, Char) -> Pipe
toPipe w (i, '|') = Pipe i (i - w, i + w)
toPipe _ (i, '-') = Pipe i (i - 1, i + 1)
toPipe w (i, 'L') = Pipe i (i - w, i + 1)
toPipe w (i, 'F') = Pipe i (i + 1, i + w)
toPipe w (i, '7') = Pipe i (i + w, i - 1)
toPipe w (i, 'J') = Pipe i (i - 1, i - w)
toPipe _ (i, 'S') = Source i
toPipe _ (i, c) = error $ "Bad pipe character " ++ [c] ++ " at pos " ++ show i

getPipe :: Board -> Int -> Pipe
getPipe board i = head $ filter (\p -> position p == i) $ pipes board

getSource :: Board -> Pipe
getSource = head . filter isSource . pipes
  where
    isSource :: Pipe -> Bool
    isSource (Pipe _ _) = False
    isSource (Source _) = True

getSourceNeighbours :: Board -> (Pipe, Pipe)
getSourceNeighbours board = getSourceNeighbours' board sources
  where
    sources = head $ filter isSource $ pipes board

    isSource :: Pipe -> Bool
    isSource (Pipe _ _) = False
    isSource (Source _) = True

    getSourceNeighbours' :: Board -> Pipe -> (Pipe, Pipe)
    getSourceNeighbours' bd s@(Source i) = (getPipe board a, getPipe board b)
      where
        w = width bd
        -- The source should always have exactly two neighbours
        neighbours = filter (\x -> any (\p -> x == position p && p `leadsTo` s) $ pipes bd) [i - 1, i - w, i + 1, i + w]
        a = head neighbours
        b = head $ tail neighbours
    getSourceNeighbours' _ _ = error "getSourceNeighbours called on non-source"

findFurthestFromSource :: Board -> Int
findFurthestFromSource b = 1 + go ((\x -> (x, x)) $ getSource b) (getSourceNeighbours b)
  where
    go :: (Pipe, Pipe) -> (Pipe, Pipe) -> Int
    go (p1, p2) cur@(c1, c2) =
      if n1 == n2
        then 1
        else 1 + go cur (n1, n2)
      where
        n1 = move b p1 c1
        n2 = move b p2 c2

move :: Board -> Pipe -> Pipe -> Pipe
move b oldP curP = head . filter (\p -> nextPos == position p) $ pipes b
  where
    (e1, e2) = exits curP
    nextPos = head $ filter (\x -> x /= position oldP) [e1, e2]

leadsTo :: Pipe -> Pipe -> Bool
leadsTo (Source _) _ = False
leadsTo p1 p2 = a == p || b == p
  where
    p = position p2
    (a, b) = exits p1

withIndex :: [a] -> [(Int, a)]
withIndex l = [0 .. length l - 1] `zip` l

onlyMainLoop :: Board -> Board
onlyMainLoop board = board {pipes = map (sourceToPipe board) $ mainLoop (getSource board) (fst $ getSourceNeighbours board)}
  where
    mainLoop :: Pipe -> Pipe -> [Pipe]
    mainLoop p1 (Source _) = [p1]
    mainLoop p1 p2 = p1 : mainLoop p2 (move board p1 p2)

sourceToPipe :: Board -> Pipe -> Pipe
sourceToPipe board (Source i) = Pipe i (bimap position position $ getSourceNeighbours board)
sourceToPipe _ p = p

hasPipe :: Board -> Int -> Bool
hasPipe board i = any (\p -> i == position p) $ pipes board

movesRight :: Pipe -> Bool
movesRight (Pipe p (e1, e2)) = e1 == p + 1 || e2 == p + 1
movesRight _ = error "Bad"

movesUp :: Pipe -> Bool
movesUp (Pipe p (e1, e2)) = e1 < p - 1 || e2 < p - 1
movesUp _ = error "Bad"

movesDown :: Pipe -> Bool
movesDown (Pipe p (e1, e2)) = e1 > p + 1 || e2 > p + 1
movesDown _ = error "Bad"

data WallEnterDirection = Up | Down | Both | None deriving (Eq, Show)

data FoldData = FoldData {wallEnterDirection :: WallEnterDirection, insideArea :: Bool, currentArea :: Int} deriving (Show)

getInnerArea :: Board -> Int
getInnerArea b = getInnerArea'
  where
    getInnerArea' :: Int
    getInnerArea' = sum $ map (\rowNum -> getRowInnerArea $ rowNum * width b) [0 .. height b - 1]

    getRowInnerArea :: Int -> Int
    getRowInnerArea rowStart = currentArea $ foldl' rowFoldStep (FoldData None False 0) [rowStart .. rowStart + width b - 1]

    rowFoldStep :: FoldData -> Int -> FoldData
    rowFoldStep (FoldData enterDir inside tot) pos
      | not isPipe && not inside = FoldData None False tot
      | not isPipe && inside = FoldData None True (tot + 1)
      -- Found a new pipe |
      | isPipe && enterDir == None && currDir == Both = FoldData None (not inside) tot
      -- Found a new pipe, must be L or F
      | isPipe && enterDir == None = FoldData currDir inside tot
      -- In a pipe, not leaving so carry on
      | isPipe && not leavingWall = FoldData enterDir inside tot
      -- L---7
      | isPipe && enterDir == Up && leavingWall && currDir == Down = FoldData None (not inside) tot
      -- L---J
      | isPipe && enterDir == Up && leavingWall && currDir == Up = FoldData None inside tot
      -- F---7
      | isPipe && enterDir == Down && leavingWall && currDir == Down = FoldData None inside tot
      -- F---J
      | isPipe && enterDir == Down && leavingWall && currDir == Up = FoldData None (not inside) tot
      -- tile after a |, the prev wall doesn't affect us
      | isPipe && enterDir == Both = rowFoldStep (FoldData None inside tot) pos
      | otherwise = error $ "no path on " ++ show (FoldData enterDir inside tot)
      where
        isPipe = b `hasPipe` pos
        pipe = getPipe b pos
        leavingWall = not $ movesRight pipe
        currDir
          | movesUp pipe && movesDown pipe = Both
          | movesUp pipe = Up
          | movesDown pipe = Down
          | otherwise = error $ show pipe

-- rowFoldStep b (n, False) i = (n, b `hasPipe` i)
-- rowFoldStep b (n, True) i
--   | b `hasPipe` i = (n, not $ movesRight $ getPipe b i)
--   | otherwise = (n + 1, True)