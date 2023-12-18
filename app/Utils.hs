{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Utils
  ( mapOneIndexed,
    withOneIndex,
    mapIndexed,
    withIndex,
    fPow,
    fPow',
    Direction (Up, Down, Left, Right),
    Position,
    Vector,
    withIndex2D,
    isOpposite,
    isAdjacent,
  )
where

import Data.Foldable (foldr')
import Data.Ix (Ix)
import Data.MemoTrie (HasTrie (..), Reg, enumerateGeneric, trieGeneric, untrieGeneric)
import GHC.Generics (Generic)
import Prelude hiding (Right, Left)

mapOneIndexed :: (Integral i) => (i -> a -> b) -> [a] -> [b]
mapOneIndexed f = map (uncurry f) . withOneIndex

withOneIndex :: (Integral i) => [a] -> [(i, a)]
withOneIndex = ([1 ..] `zip`)

mapIndexed :: (Integral i) => (i -> a -> b) -> [a] -> [b]
mapIndexed f = map (uncurry f) . withIndex

withIndex :: (Integral i) => [a] -> [(i, a)]
withIndex = ([0 ..] `zip`)

withIndex2D :: (Integral i) => [[a]] -> [[((i,i),a)]]
withIndex2D = mapIndexed (\i -> mapIndexed (\j -> ((i, j),)))

-- Not sure which of these is better? I guess the second but left both here and can't be bothered to test
fPow :: Int -> (a -> a) -> (a -> a)
fPow n f = foldr' (.) id $ replicate n f

fPow' :: Int -> (a -> a) -> a -> a
fPow' n f a = iterate f a !! n

data Direction = Up | Down | Left | Right deriving (Show, Eq, Ord, Ix, Generic)

type Position = (Int, Int)

type Vector = (Position, Direction)

instance HasTrie Direction where
  newtype Direction :->: b = DirTree {unDirTree :: Reg Direction :->: b}
  trie = trieGeneric DirTree
  untrie = untrieGeneric unDirTree
  enumerate = enumerateGeneric unDirTree


isOpposite :: Direction -> Direction -> Bool
isOpposite Up Down = True
isOpposite Down Up = True
isOpposite Left Right = True
isOpposite Right Left = True
isOpposite _ _ = False

isAdjacent :: Direction -> Direction -> Bool
isAdjacent Left Right = False
isAdjacent Left Left = False
isAdjacent Right Left = False
isAdjacent Right Right = False
isAdjacent Up Down = False
isAdjacent Up Up = False
isAdjacent Down Up = False
isAdjacent Down Down = False
isAdjacent _ _ = True