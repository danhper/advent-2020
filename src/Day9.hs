module Day9 (
    solve,
) where

import qualified Data.IntMultiSet as MS
import Data.Maybe (listToMaybe)
import qualified Deque.Strict as Q

import Utils (formatResults, parseIntegersList)

findSum :: Int -> MS.IntMultiSet -> Maybe (Int, Int)
findSum target numbers = transformResult <$> result
  where
    findNumber x = MS.member (target - x) (MS.delete x numbers)
    result = listToMaybe $ MS.toAscList $ MS.filter findNumber numbers
    transformResult r = (r, target - r)

findWrongNumber :: Int -> [Int] -> Int
findWrongNumber elemsSize numbers =
    findWrongNumber' (drop elemsSize numbers) (MS.fromList initialNumbers) initialSeen
  where
    initialNumbers = take elemsSize numbers
    initialSeen = Q.fromConsAndSnocLists initialNumbers []
    findWrongNumber' [] _ _ = error "wrong number not found"
    findWrongNumber' (x : xs) available seen = maybe x (const loop) (findSum x available)
      where
        Just (elem, newSeen) = Q.uncons seen
        newAvailable = MS.insert x (MS.delete elem available)
        loop = findWrongNumber' xs newAvailable (Q.snoc x newSeen)

findContiguous :: Int -> [Int] -> Int
findContiguous target numbers = findContiguous' (tail numbers) (Q.fromConsAndSnocLists [head numbers] [])
  where
    findContiguous' [] _ = error "target not found"
    findContiguous' (x : xs) seen
        | sum seen == target = minimum seen + maximum seen
        | sum seen > target = findContiguous' (x : xs) (Q.tail seen)
        | otherwise = findContiguous' xs (Q.snoc x seen)

solve :: String -> String
solve content = formatResults part1 part2
  where
    numbers = parseIntegersList content
    part1 = findWrongNumber 25 numbers
    part2 = findContiguous part1 numbers
