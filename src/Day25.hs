module Day25 (
    solve,
) where

import Utils (formatResults)

step :: Int -> Int -> Int
step value subject = (value * subject) `mod` 20201227

findLoopSize :: Int -> Int
findLoopSize target = findLoopSize' 1 0
  where
    findLoopSize' value loopSize
        | value == target = loopSize
        | otherwise = findLoopSize' (step value 7) (loopSize + 1)

computeValue :: Int -> Int -> Int
computeValue subject = computeValue' 1
  where
    computeValue' value 0 = value
    computeValue' value loopSize = computeValue' (step value subject) (loopSize - 1)

solve :: String -> String
solve content = formatResults part1 part2
  where
    [cardPublicKey, doorPublicKey] = map read $ lines content
    part1 = computeValue doorPublicKey (findLoopSize cardPublicKey)
    part2 = "All done!"
