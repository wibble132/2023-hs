{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day22 where

import Data.Bifunctor (first, second)
import Data.List (foldl', group, sort, sortOn, intersect)
import Data.List.Extra (groupSort, lastDef, splitOn)
import Data.Map (Map, assocs, empty, fromAscList, insert, keys, update)
import qualified Data.Map as Map
import Debug.Trace (trace)
import GHC.Arr (Array, listArray, (!), (//))
import GHC.Base (liftM2)
import Utils (Pos3, Position, toPos3, xPos, yPos, zPos)

-- 711 too high with HeightMap
-- 475 too low with settling - bad intersection testing
-- 477 correct with settling 
part1 :: String -> String
part1 s =
  show (length canBeRemoved2)
  where
    blocks :: [Block]
    blocks = sortOn (zPos . start) $ parseInput s

    blockIds = map bId blocks

    (_, settledGraph) = fullySettleBlocks blocks
    supports2 = Map.elems settledGraph
    canBeRemoved2 = filter (\i -> [i] `notElem` supports2) blockIds

part2 :: String -> String
part2 s = show $ sum $ map (countWillFall blockGraph) willFalls
  where
    blocks :: [Block]
    blocks = sortOn (zPos . start) $ parseInput s
    blockIds = map bId blocks

    (_, blockGraph) = fullySettleBlocks blocks
    
    willFalls = filter (\i -> [i] `elem` Map.elems blockGraph) blockIds

type BlockId = Int

data Block = Block {start :: Pos3, end :: Pos3, bId :: BlockId} deriving (Show, Eq, Read)

type Height = Int

type HeightMap = Array Position (Height, BlockId)

type SupportGraph = Map BlockId [BlockId]

parseInput :: String -> [Block]
parseInput = map (uncurry pos3sToBlock . first lineToPos3s) . (`zip` [0 ..]) . lines
  where
    lineToPos3s :: String -> [Pos3]
    lineToPos3s = map (toPos3 . (map (read :: String -> Int) . splitOn ",")) . splitOn "~"

    pos3sToBlock :: [Pos3] -> BlockId -> Block
    pos3sToBlock p@[(x1, y1, z1), (x2, y2, z2)] _
      | x1 > x2 || y1 > y2 || z1 > z2 = error $ show p
    pos3sToBlock [x, y] i = Block x y i
    pos3sToBlock _ _ = error "Bad pair"

groundTiles :: Block -> [Position]
groundTiles (Block (x1, y1, _) (x2, y2, _) _) = [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]

getEmptyHeightMap :: [Block] -> HeightMap
getEmptyHeightMap blocks = listArray ((xMin, yMin), (xMax, yMax)) (repeat (0, -1))
  where
    xMin = minimum $ map (xPos . start) blocks
    yMin = minimum $ map (yPos . start) blocks
    xMax = maximum $ map (xPos . end) blocks
    yMax = maximum $ map (yPos . end) blocks

dropBlockOntoMap :: HeightMap -> Block -> (HeightMap, [BlockId])
dropBlockOntoMap heightMap b = (updatedHeights, restingBlocks)
  where
    -- The points in the 2D projection of the block onto the ground
    groundArea :: [Position]
    groundArea = groundTiles b
    -- List of the heights of the ground below the block, and the id of the block on top of that position
    underHeights :: [(Height, BlockId)]
    underHeights = sortOn fst $ map (heightMap !) groundArea
    -- The tallest height on the ground below the block
    restingHeight :: Height
    restingHeight = maximum $ map fst underHeights
    -- The id of the blocks at the maximum height below this block
    restingBlocks :: [BlockId]
    restingBlocks = filter (/= -1) . map snd $ filter ((== restingHeight) . fst) underHeights
    bHeight :: Height
    bHeight =
      if zPos (start b) <= restingHeight
        then error "bad bloc"
        else zPos (end b) - zPos (start b) + 1
    updatedHeights :: HeightMap
    updatedHeights = heightMap // map (,(restingHeight + bHeight, bId b)) groundArea

dropAllBlocks :: [Block] -> (HeightMap, SupportGraph)
dropAllBlocks bs = foldl' foldStep (getEmptyHeightMap bs, fromAscList [(i, []) | i <- [0 .. length bs - 1]]) bs
  where
    foldStep :: (HeightMap, Map BlockId [BlockId]) -> Block -> (HeightMap, Map BlockId [BlockId])
    foldStep (heightMap, m) b = second (\supports -> update (Just . (++ supports)) (bId b) m) $ dropBlockOntoMap heightMap b

_reverseGraph :: SupportGraph -> SupportGraph
_reverseGraph m = fromAscList (map (\k -> (k, map fst $ filter ((k `elem`) . snd) $ assocs m)) (keys m))

fullySettleBlocks :: [Block] -> ([Block], Map BlockId [BlockId])
fullySettleBlocks bs = if settledBs == bs then res else trace "Bad settle" fullySettleBlocks settledBs
  where
    res@(settledBs, _) = settleBlocks bs

settleBlocks :: [Block] -> ([Block], Map BlockId [BlockId])
settleBlocks blocks = foldl' f ([], empty) sortedBlocks
  where
    sortedBlocks = sortOn (zPos . start) blocks
    f (acc, deps) block = (acc ++ [b], insert (bId b) bIds deps)
      where
        (b, bIds) = settleBlock acc block

settleBlock :: [Block] -> Block -> (Block, [BlockId])
settleBlock blocks block =
  -- trace
  --   (show (bId block, map bId restingBlocks))
  (block `onTopOf` restingHeight, map bId restingBlocks)
  where
    blocksBelow =
      groupSort
        $ map ((,) =<< (zPos . end))
        $ filter
          ( \b ->
              not $ null $ intersect (groundTiles b) (groundTiles block)
          )
        $ filter
          (\b -> zPos (end b) < zPos (start block))
          blocks
    (restingHeight, restingBlocks) = lastDef (0, []) blocksBelow

removeDuplicates :: (Eq a, Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

onTopOf :: Block -> Height -> Block
onTopOf b restingHeight =
  b
    { start = start b `withHeight` (restingHeight + 1),
      end = end b `withHeight` (restingHeight + blockHeight b + 1)
    }

withHeight :: Pos3 -> Height -> Pos3
withHeight = flip (trimap id id . const)

trimap :: (a0 -> b0) -> (a1 -> b1) -> (a2 -> b2) -> (a0, a1, a2) -> (b0, b1, b2)
trimap f0 f1 f2 (a0, a1, a2) = (f0 a0, f1 a1, f2 a2)

blockHeight :: Block -> Height
blockHeight = liftM2 (-) (zPos . end) (zPos . start)

removeAt :: [a] -> Int -> [a]
removeAt [] _ = []
removeAt (_ : ls) 0 = ls
removeAt (l : ls) x = l : ls `removeAt` (x - 1)

eqById :: [Block] -> [Block] -> Bool
eqById b1 b2 = sortOn bId b1 == sortOn bId b2

getById :: [Block] -> BlockId -> Block
getById bs i = head . filter ((== i) . bId) $ bs

countWillFall :: SupportGraph -> BlockId -> Int
countWillFall graph bi = length (getFalling [bi]) - 1
  where
    reversed = _reverseGraph graph

    getFalling :: [BlockId] -> [BlockId]
    getFalling ids
      | null nextAbove = ids
      | otherwise = getFalling $ removeDuplicates (ids ++ nextAbove)
      where
        nextAbove = getFallingStep ids

    getFallingStep :: [BlockId] -> [BlockId]
    getFallingStep supports = concatMap (filter (\i -> (i `notElem` supports) && (graph Map.! i `isSubset` supports)) . (reversed Map.!)) supports

isSubset :: Eq a => [a] -> [a] -> Bool
isSubset l1 l2 = intersect l1 l2 == l1