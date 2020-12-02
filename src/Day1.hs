{-# LANGUAGE OverloadedStrings #-}

module Day1
  ( run,
  )
where

import Data.List (find)
import qualified Data.Set as S
import Formatting (formatToString, int, (%))
import Utils (inputFile, integersFromFile)

solveDay1 :: [Int] -> (Int, Int)
solveDay1 numbers = (a1 * b1, a2 * b2 * (target - (a2 + b2)))
  where
    cartesian = [(x, y, x + y) | x <- numbers, y <- numbers]
    Just (a1, b1, _) = find (\(_, _, v) -> v == target) cartesian
    numbersSet = foldr S.insert S.empty numbers
    Just (a2, b2, _) = find (\(_, _, v) -> S.member (target - v) numbersSet) cartesian
    target = 2020

run :: IO String
run = do
  numbers <- integersFromFile $ inputFile "day1.txt"
  let (part1, part2) = solveDay1 numbers
  return $ formatToString ("Part 1: " % int % "\nPart 2: " % int % "\n") part1 part2
