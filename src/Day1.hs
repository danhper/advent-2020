module Day1
  ( solve,
  )
where

import Data.List (find)
import qualified Data.Set as S
import Utils
  ( formatIntResults,
    parseIntegersList,
  )

computeAnswer :: [Int] -> (Int, Int)
computeAnswer numbers = (a1 * b1, a2 * b2 * (target - (a2 + b2)))
  where
    cartesian = [(x, y, x + y) | x <- numbers, y <- numbers]
    Just (a1, b1, _) = find (\(_, _, v) -> v == target) cartesian
    numbersSet = foldr S.insert S.empty numbers
    Just (a2, b2, _) =
      find (\(_, _, v) -> S.member (target - v) numbersSet) cartesian
    target = 2020

solve :: String -> String
solve content = formatIntResults part1 part2
  where
    numbers = parseIntegersList content
    (part1, part2) = computeAnswer numbers
