module Day10 (
    solve,
) where

import Utils (
    formatResults,
    parseIntegersList,
 )

import Data.List (sort)
import qualified Data.Map as M

computeDifferences :: [Int] -> M.Map Int Int
computeDifferences numbers = snd $ foldl addDifference (head numbers, M.empty) $ tail numbers
  where
    addDifference (previous, results) value = (value, M.insertWith (const (+ 1)) (value - previous) 1 results)

arrangements :: [Int] -> Int
arrangements numbers = fst $ computeCombinations numbers M.empty
  where
    computeCandidates x xs = [0 .. length (filter (<= x + 3) $ take 3 xs) - 1]
    computeCombinations [_] memo = (1, memo)
    computeCombinations (x : xs) memo
        | M.member x memo = (memo M.! x, memo)
        | otherwise = (res, M.insert x res newMemo)
      where
        (res, newMemo) = foldl handleCandidate (0, memo) (computeCandidates x xs)
        handleCandidate (v, newMemo) skip = let (nextV, m) = computeCombinations (drop skip xs) newMemo in (v + nextV, m)

solve :: String -> String
solve content = formatResults part1 part2
  where
    numbers = sort $ parseIntegersList content
    allNumbers = 0 : numbers ++ [last numbers + 3]
    differences = computeDifferences allNumbers
    part1 = (differences M.! 1) * (differences M.! 3)
    part2 = arrangements allNumbers
