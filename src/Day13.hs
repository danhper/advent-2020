module Day13 (
    solve,
) where

import Data.Bifunctor (second)
import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Ord (comparing)
import Text.Read (readMaybe)
import Utils (formatIntResults)

solvePart1 :: String -> Int
solvePart1 content = bus * wait
  where
    [rawTime, rawBuses] = lines content
    (time, buses) = (read rawTime, mapMaybe readMaybe $ splitOn "," rawBuses)
    bestTime = map (\bus -> (bus, getBestDeparture bus - time)) buses
    (bus, wait) = minimumBy (comparing snd) bestTime
    getBestDeparture bus = last $ takeWhile (\x -> x - bus <= time) [bus, bus + bus ..]

egcd :: Int -> Int -> (Int, Int)
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
    (s, t) = egcd b r
    (q, r) = a `quotRem` b

modInv :: Int -> Int -> Int
modInv a b = fst $ egcd a b

chineseRemainder :: [Int] -> [Int] -> Int
chineseRemainder residues modulii = f $ zipWith modInv crtModulii modulii
  where
    f = (`mod` modPI) . sum . zipWith (*) crtModulii . zipWith (*) residues
    modPI = product modulii
    crtModulii = (modPI `div`) <$> modulii

solvePart2 :: String -> Int
solvePart2 content = chineseRemainder a n
  where
    numbers = map readMaybe $ splitOn "," (last $ lines content)
    constraints = map (getMod . second fromJust) $ filter (isJust . snd) $ zip [0 ..] numbers
    getMod (a, b) = ((- a) `mod` b, b)
    (a, n) = unzip constraints

solve :: String -> String
solve content = formatIntResults part1 part2
  where
    part1 = solvePart1 content
    part2 = solvePart2 content
