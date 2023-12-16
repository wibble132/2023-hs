module Utils
  ( mapOneIndexed,
    withOneIndex,
    mapIndexed,
    withIndex,
    fPow,
    fPow',
  )
where

import Data.Foldable (foldr')

mapOneIndexed :: (Integral i) => (i -> a -> b) -> [a] -> [b]
mapOneIndexed f = map (uncurry f) . withOneIndex

withOneIndex :: (Integral i) => [a] -> [(i, a)]
withOneIndex = ([1 ..] `zip`)

mapIndexed :: (Integral i) => (i -> a -> b) -> [a] -> [b]
mapIndexed f = map (uncurry f) . withIndex

withIndex :: (Integral i) => [a] -> [(i, a)]
withIndex l = [0 ..] `zip` l

-- Not sure which of these is better? I guess the second but left both here and can't be bothered to test
fPow :: Int -> (a -> a) -> (a -> a)
fPow n f = foldr' (.) id $ replicate n f

fPow' :: Int -> (a -> a) -> a -> a
fPow' n f a = iterate f a !! n
