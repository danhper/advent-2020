{-# LANGUAGE BangPatterns #-}

module Day23 (
    solve,
) where

import Data.Char (digitToInt)
import Data.Foldable (find)
import Data.List (elemIndex, intercalate, sort)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Debug.Trace (trace)
import Utils (formatResults)

type CircularList = M.Map Int Int

data GameState = GameState
    { current :: Int
    , list :: CircularList
    , maxValue :: Int
    }

instance Show GameState where
    show state@(GameState current _ _) = intercalate " -> " $ map show' $ toList state
      where
        show' v
            | current == v = "(" ++ show v ++ ")"
            | otherwise = show v

showCompact :: Int -> GameState -> String
showCompact n state = intercalate "" $ map show $ toList state{current = n}

toList :: GameState -> [Int]
toList (GameState current list _) = toList' current list
  where
    toList' current list
        | M.null list = []
        | otherwise = current : toList' (list M.! current) (M.delete current list)

nexts :: GameState -> [Int]
nexts state = take 3 $ drop 1 $ toList state

removeNexts :: GameState -> (GameState, [(Int, Int)])
removeNexts state@(GameState current list _) = (state{list = nextMap}, removed)
  where
    nextKeys = nexts state
    removedMap = foldl (\m k -> M.insert k (list M.! k) m) M.empty nextKeys
    newMap = foldl (flip M.delete) list nextKeys
    nextMap = M.insert current (list M.! last nextKeys) newMap
    removed = map (\c -> (c, list M.! c)) nextKeys

insertCups :: GameState -> Int -> [(Int, Int)] -> GameState
insertCups state@(GameState _ list _) destination cups = state{list = newList}
  where
    previousValue = list M.! destination
    transformedCups = M.insert (fst $ last cups) previousValue $ M.fromList cups
    newList = M.union transformedCups (M.insert destination (fst $ head cups) list)

doRound :: GameState -> GameState
doRound state = withInserted{current = list withInserted M.! current withInserted}
  where
    (newState, removedCups) = removeNexts state
    destination = findDestination newState
    withInserted = insertCups newState destination removedCups

findDestination :: GameState -> Int
findDestination state@(GameState current list maxValue) = findDestination' (current - 1)
  where
    findDestination' candidate
        | candidate `M.member` list = candidate
        | otherwise = findDestination' nextCandidate
      where
        nextCandidate = if candidate > 1 then candidate - 1 else maxValue

fromList :: Int -> String -> GameState
fromList total content = GameState (head allNumbers) numbersList total
  where
    numbers = map digitToInt content
    allNumbers = numbers ++ [maxNum + 1, maxNum + 2 .. total]
    maxNum = maximum numbers
    numbersList = M.fromList $ zip allNumbers $ tail allNumbers ++ [head allNumbers]

getMultipliedCups :: GameState -> Int
getMultipliedCups (GameState _ final _) = firstCup * secondCup
  where
    firstCup = final M.! 1
    secondCup = final M.! firstCup

doRounds :: Int -> GameState -> GameState
doRounds 0 state = state
doRounds n state = doRounds (n - 1) nextStateWithLog
  where
    !nextState = doRound state
    !nextStateWithLog
        | n `mod` 10000 == 0 = trace (show n ++ ": " ++ show (getMultipliedCups nextState)) nextState
        | otherwise = nextState

solvePart2 :: String -> Int
solvePart2 content = getMultipliedCups finalState
  where
    initialState = fromList 1000000 content
    finalState = doRounds 10000000 initialState

solve :: String -> String
solve content = formatResults part1 part2
  where
    part1 = tail $ showCompact 1 $ doRounds 100 $ fromList (length content) content
    part2 = solvePart2 content
