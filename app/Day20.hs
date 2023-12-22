{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Day20 (part1, part2) where

import Data.Bifunctor (first, second)
import Data.Foldable (foldl', toList)
import Data.List (elemIndex, isPrefixOf, sortBy)
import Data.List.Extra (breakOn)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down), comparing)
import Data.Vector (Vector, fromList, replicate, (//))
import Data.Vector.Generic ((!))
import Debug.Trace (trace)
import Utils (withIndex)

part1 :: String -> String
part1 s = show (a * b)
  where
    (a, b, _, _) = iterate (pushButton modules) (0, 0, dat, 0) !! 20000
    (modules, dat) = parseInput s

-- 238593356738827
part2 :: String -> String
part2 s = show $ foldl' lcm 1 ([4027, 3769, 3929, 4001] :: [Integer])
  where
    (modules, dat) = parseInput s
    rx = last modules
    bq = modules !! head (getInputs modules rx)
    importants = getInputs modules bq

conjStateToChar :: ConjunctionState -> Char
conjStateToChar Disconnected = '_'
conjStateToChar ConjHigh = '1'
conjStateToChar ConjLow = '0'

conjString :: Vector ConjunctionState -> String
conjString = map conjStateToChar . toList

pushButton :: [Module] -> (Int, Int, Data, Int) -> (Int, Int, Data, Int)
pushButton ms (lows, highs, dat, buttonCount) = (lows + newLows + 1, highs + newHighs, nextDat, buttonCount + 1)
  where
    broadcaster = head ms
    initialPulse = [(Low (-1), mId broadcaster)]

    (newLows, newHighs, nextDat) = goLoop initialPulse dat

    goLoop :: [(Pulse, MID)] -> Data -> (Int, Int, Data)
    goLoop ps _dat = (low, high, datNew)
      where
        (_, low, high, datNew, _) = head $ Prelude.filter isFinished $ iterate goStep (ps, 0, 0, _dat, 0)

        isFinished ([], _, _, _, _) = True
        isFinished _ = False

    goStep :: ([(Pulse, MID)], Int, Int, Data, Int) -> ([(Pulse, MID)], Int, Int, Data, Int)
    goStep  (ps, _lows, _highs, _dat, step) = (nextPulses, ls, hs, newDat, step + 1)
      where
        (nextPulses, newDat) =
          foldl'
            ( \(acc, dat') (p, i) ->
                if i == -1 then (acc, dat') else first (acc ++) $ applyPulse' (buttonCount,step) p (ms !! i) dat'
            )
            ([], _dat)
            ps
        ls = _lows + length (Prelude.filter (isLow . fst) nextPulses)
        hs = _highs + length (Prelude.filter (isHigh . fst) nextPulses)

type MID = Int

data Module
  = FlipFlop {destinations :: [Int], mId :: MID} -- % - low -> flip state and sends (if on then high else low),  high -> nothing
  | Conjunction {destinations :: [Int], mId :: MID} -- & - update memory of pulse, then if ALL high -> low, otherwise -> high
  | Broadcaster {destinations :: [Int], mId :: MID} -- forwards the pulse onwards to all
  | RX {destinations :: [Int], mId :: MID}
  deriving (Show)

isConjunction :: Module -> Bool
isConjunction (Conjunction {}) = True
isConjunction _ = False

getInputs :: [Module] -> Module -> [Int]
getInputs _ (Broadcaster {}) = []
getInputs ms m = map mId $ Prelude.filter ((mId m `elem`) . destinations) ms

type Data = (Vector State, Vector (Vector ConjunctionState))

parseInput :: String -> ([Module], Data)
parseInput s = (modules, (baseData, conjunctionData))
  where
    ss = sortBy (comparing Data.Ord.Down) (lines s)
    names = "broadcaster" : map tail (Prelude.filter ((\x -> x == '%' || x == '&') . head) $ map (takeWhile (/= ' ')) ss) ++ ["rx"]
    modules = map (parseModule names) ss ++ [RX [] $ length names - 1]
    moduleCount = length names
    baseData = Data.Vector.replicate (length names) Off
    conjunctionData = fromList $ map (\m -> fromList $ if isConjunction m then map ((\b -> if b then ConjLow else Disconnected) . (`elem` getInputs modules m)) [0 .. moduleCount - 1] else Prelude.replicate moduleCount Disconnected) modules

parseModule :: [String] -> String -> Module
parseModule names s
  | head s == '%' = FlipFlop dests $ getId name
  | head s == '&' = Conjunction dests $ getId name
  | "broadcaster " `isPrefixOf` s = Broadcaster dests 0
  | otherwise = error "Bad input"
  where
    (defn, destString) = breakOn " -> " s
    name = if head defn == 'b' then defn else tail defn
    dests = map getId $ splitOn ", " $ drop 4 destString
    -- If it isn't a module in the list, then just send it to -1
    getId = fromMaybe (-1) . (`elemIndex` names)

data Pulse = Low MID | High MID deriving (Eq, Show)

isLow :: Pulse -> Bool
isLow (Low _) = True
isLow (High _) = False

isHigh :: Pulse -> Bool
isHigh (Low _) = False
isHigh (High _) = True

data State = On | Off deriving (Eq, Show)

data ConjunctionState = Disconnected | ConjLow | ConjHigh deriving (Eq, Show)

-- Wrapper around applyPulse to trace output some debugging info
applyPulse' :: (Int, Int) -> Pulse -> Module -> Data -> ([(Pulse, Int)], Data)
applyPulse' (btnNum, step) (Low _) (Conjunction _ i)
  | i == 7 && trace (show (btnNum, step, i)) False = undefined
applyPulse' _ a b = applyPulse a b

applyPulse :: Pulse -> Module -> Data -> ([(Pulse, Int)], Data)
applyPulse (Low _) (FlipFlop dests i) dat = case fst dat ! i of
  Off -> (map (High i,) dests, first (// [(i, On)]) dat)
  On -> (map (Low i,) dests, first (// [(i, Off)]) dat)
applyPulse (High _) (FlipFlop _ _) dat = ([], dat)
applyPulse (Low from) (Conjunction dests i) dat = case ddat ! from of
  Disconnected -> error "Bad"
  ConjLow -> (map (High i,) dests, dat)
  ConjHigh -> (map (High i,) dests, second (// [(i, changedDdat)]) dat)
  where
    ddat = snd dat ! i
    changedDdat = ddat // [(from, ConjLow)]
applyPulse (High from) (Conjunction dests i) dat = case ddat ! from of
  Disconnected -> error "Bad"
  ConjLow -> (map (p1 i,) dests, second (// [(i, changedDdat)]) dat)
  ConjHigh -> (map (p2 i,) dests, dat)
  where
    ddat = snd dat ! i
    changedDdat = ddat // [(from, ConjHigh)]
    p1 = if ConjLow `notElem` changedDdat then Low else High
    p2 = if ConjLow `notElem` ddat then Low else High
applyPulse (High _) (Broadcaster dests i) dat = (map (High i,) dests, dat)
applyPulse (Low _) (Broadcaster dests i) dat = (map (Low i,) dests, dat)
applyPulse (High _) (RX _ _) dat = ([], dat)
applyPulse (Low _) (RX _ i) dat = ([], first (// [(i, On)]) dat)

withNext :: [a] -> [(a, a)]
withNext (a : rest@(b : _)) = (a, b) : withNext rest
withNext [_] = []
withNext [] = []

--- Manual nonsense for part 2
{--

RX has an input of a Conjunction node
This node has 4 inputs
Each input comes from an independent section of the graph of modules.

day20-e3.txt
1. LH, DL, GB, CP, BM, FN, GF, KH, XM, HF, ZX, LG  --> &LX --> &VG --> &BQ --> RX

day20-e4.txt
2.

--}