module Day08 (part1, part2) where

import Data.Foldable (foldl')
import Data.List (elemIndex, findIndices)
import Data.Maybe (fromJust)

-- 13939 Correct :)
part1 :: String -> String
part1 s = show $ countMovesToZZZ (nodes input) (cycle $ moves input) (getNodeWithName (nodes input) "AAA")
  where
    input = parseInput s

-- 3500635456761298348877129 too high (no surprise hehe)
-- 3500635456761298348877129 fixed bug, but same answer as above so didn't submit
-- 8906539031197 Correct, worked by hand based on helper functions here
--               "Fixed" code, got this result. I was trying to make the code workd more generally than needed for this particular input
part2 :: String -> String
-- part2 s = show . getFinishIntersection . map (getCycle (nodes input) (moves input)) $ filter (endsWith 'A') $ nodes input
-- part2 s = unlines . map (show . (\x -> (x, getCycle (nodes input) (moves input) x))) $ filter (endsWith 'A') $ nodes input
part2 s = show . getFinishIntersection . map (getCycle (nodes input) (moves input)) $ filter (endsWith 'A') $ nodes input
-- part2 s = show $ test input
  where
    input = parseInput s

data Input = Input {moves :: [Move], nodes :: [Node]} deriving (Show)

data Move = MLeft | MRight deriving (Eq)

instance Show Move where
  show MLeft = "L"
  show MRight = "R"

-- Use NodeName as an initialisation step
data Node = Node {name :: String, left :: String, right :: String}

instance Show Node where
  -- show (Node n l r) = n ++ " = (" ++ l ++ ", " ++ r ++ ")"
  show (Node n _ _) = n -- ++ " = (" ++ l ++ ", " ++ r ++ ")"

instance Eq Node where
  (==) n1 n2 = name n1 == name n2

parseInput :: String -> Input
parseInput s = case lines s of
  (ms : _ : ns) -> Input (parseMoves ms) (map (wordsToNode . words) ns)
  _ -> Input [] []

parseMoves :: String -> [Move]
parseMoves [] = []
parseMoves ('R' : cs) = MRight : parseMoves cs
parseMoves ('L' : cs) = MLeft : parseMoves cs
parseMoves c = error ("Bad move" ++ show c)

wordsToNode :: [String] -> Node
wordsToNode (n : "=" : l : r : _) = Node n (tail $ init l) (init r)
wordsToNode s = error $ "Bad node name " ++ unwords s

getNodeWithName :: [Node] -> String -> Node
getNodeWithName (n : ns) s
  | name n == s = n
  | otherwise = getNodeWithName ns s
getNodeWithName [] s = error $ "Couldn't get node with name " ++ s

takeMove :: [Node] -> Node -> Move -> Node
takeMove ns n MLeft = getNodeWithName ns $ left n
takeMove ns n MRight = getNodeWithName ns $ right n

takeMovesAll :: [Node] -> [Move] -> Node -> [Node]
takeMovesAll ns ms n = reverse $ foldl' (\acc m -> takeMove ns (head acc) m : acc) [n] ms

countMovesToZZZ :: [Node] -> [Move] -> Node -> Integer
countMovesToZZZ ns (m : ms) n
  | name n == "ZZZ" = 0
  | otherwise = 1 + countMovesToZZZ ns ms (takeMove ns n m)
countMovesToZZZ _ [] _ = error "No moves given to countMovesToZZZ"

endsWith :: Char -> Node -> Bool
endsWith c n = c == last (name n)

data Cycle = Cycle {cycleLength :: Integer, finishIndex :: Integer} deriving (Show)

getCycle :: [Node] -> [Move] -> Node -> Cycle
getCycle ns ms n = Cycle cycleLen (head finishIdxs)
  where
    (mvs, nCycleStart) = moveUntilCycle ns ms [] n
    nodesList = flattenCycleMoves mvs
    firstIndex = toInteger $ fromJust $ nCycleStart `elemIndex` nodesList
    finishIdxs = map toInteger $ findIndices (endsWith 'Z') nodesList
    -- This doesn't work in the example due to **r e a s o n s** and the offset at the end works for some reason...
      -- (I do know why it doesn't work in example, and it only works on the input because it is a special case)
      -- (The general problem is much harder, and because this is AoC we only care about the one input)
    cycleLen = toInteger (length nodesList) - firstIndex

moveUntilCycle :: [Node] -> [Move] -> [[Node]] -> Node -> ([[Node]], Node)
moveUntilCycle ns ms prevs n =
  if n `elem` map head prevs
    then (reverse prevs, n)
    else moveUntilCycle ns ms (next : prevs) nextFinish
  where
    next = takeMovesAll ns ms n
    nextFinish = last next

flattenCycleMoves :: [[Node]] -> [Node]
flattenCycleMoves = foldl' (\acc x -> acc ++ init x) []

-- something is wrong somewhere in multiple places, so i'm doing it like this
getFinishIntersection :: [Cycle] -> Integer
getFinishIntersection = foldl' lcm 1 . map finishIndex

-- Chinese Remainder Theorem solver
-- I thought this was needed for finding the intersecion, but it turns out that the 
--  finish points are singly at the end of the cycles so it suffices to find the lcm of them all as above
_crt :: [(Integer, Integer)] -> (Integer, Integer)
_crt = foldr go (0, 1)
  where
    go (r1, m1) (r2, m2) = (r `mod` m, m)
      where
        r = r2 + m2 * (r1 - r2) * (m2 `modInv` m1)
        m = m2 * m1

    modInv :: Integer -> Integer -> Integer
    a `modInv` m = let (_, i, _) = gcd' a m in i `mod` m

    gcd' :: Integer -> Integer -> (Integer, Integer, Integer)
    gcd' 0 b = (b, 0, 1)
    gcd' a b = (g, t - (b `div` a) * s, s)
      where
        (g, s, t) = gcd' (b `mod` a) a