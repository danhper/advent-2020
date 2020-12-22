module Day14 (
    solve,
) where

import qualified Data.Map as M
import Text.Parsec (Parsec)
import Text.ParserCombinators.Parsec (digit, many1, parse, string)
import Utils (formatResults, readBin, showBin, showPaddedBin)

type Memory = M.Map Int Int
type Mask = M.Map Int Char

memorySetter :: Parsec String u (Int, Int)
memorySetter = (,) <$> (string "mem[" *> number) <*> (string "] = " *> number)
  where
    number = read <$> many1 digit

parseMemorySetter :: String -> (Int, Int)
parseMemorySetter value = result
  where
    Right result = parse memorySetter "(failed)" value

parseMask :: String -> Mask
parseMask mask = foldl processChar M.empty (zip [0 ..] $ reverse $ drop (length "mask = ") mask)
  where
    processChar maskMap (index, c) = M.insert index c maskMap

applyMask :: Int -> Mask -> Int
applyMask value mask = readBin binString
  where
    normalizedMask = M.filter (/= 'X') mask
    binValues = M.fromList $ zip [0 ..] $ reverse $ showBin value
    binString = reverse $ M.elems $ M.unions [normalizedMask, binValues, M.fromList $ zip [0 .. 36] (repeat '0')]

expandMask :: Mask -> [Mask]
expandMask mask = map (replaceMask normalizedMask xs) allValues
  where
    xs = M.keys $ M.filter (== 'X') mask
    normalizedMask = M.filter (/= '0') mask
    replaceMask mask xs values = foldl (\m (i, v) -> M.insert i v m) mask $ zip xs values
    allValues = map (showPaddedBin $ length xs) [0 .. 2 ^ length xs - 1]

initializeMemory :: [String] -> Memory
initializeMemory lines = fst $ foldl processLine (M.empty, M.empty) lines
  where
    processLine (mem, _) line@('m' : 'a' : _) = (mem, parseMask line)
    processLine (mem, mask) line = (udpateMemory mask (parseMemorySetter line) mem, mask)
    udpateMemory mask (index, value) = M.insert index (applyMask value mask)

initializeMemoryV2 :: [String] -> Memory
initializeMemoryV2 lines = fst $ foldl processLine (M.empty, []) lines
  where
    processLine (mem, _) line@('m' : 'a' : _) = (mem, expandMask $ parseMask line)
    processLine (mem, masks) line = (foldl (\m i -> M.insert i value m) mem locations, masks)
      where
        (index, value) = parseMemorySetter line
        locations = map (applyMask index) masks

solve :: String -> String
solve content = formatResults part1 part2
  where
    part1 = sum $ M.elems $ initializeMemory $ lines content
    part2 = sum $ M.elems $ initializeMemoryV2 $ lines content
