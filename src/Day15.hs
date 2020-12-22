module Day15 (
    solve,
) where

import qualified Data.HashMap.Strict as M
import Data.List.Split (splitOn)
import Utils (formatResults)

type SeenNumbers = M.HashMap Int [Int]
data GameState = GameState
    { lastSpoken :: Int
    , numbers :: SeenNumbers
    , turn :: Int
    }
    deriving (Show)

addNumber :: Int -> Int -> SeenNumbers -> SeenNumbers
addNumber turn number numbers = M.insert number next numbers
  where
    current = M.lookupDefault [] number numbers
    next = case current of
        [] -> [turn]
        (x : _) -> [turn, x]

getSpoken :: GameState -> Int
getSpoken state = case numbers state M.! lastSpoken state of
    [_] -> 0
    (a : b : _) -> a - b

runGame :: Int -> GameState -> GameState
runGame turns = runGame'
  where
    runGame' state
        | turn state == turns = state
        | otherwise = runGame' $ GameState{lastSpoken = spoken, numbers = nextNumbers, turn = turn state + 1}
      where
        spoken = getSpoken state
        nextNumbers = addNumber (turn state) spoken (numbers state)

initializeState :: [Int] -> GameState
initializeState initSeq = initializeState' initSeq initialState
  where
    initialState = GameState{turn = 1, numbers = M.empty, lastSpoken = 0}
    initializeState' [] state = state
    initializeState' (x : xs) state = initializeState' xs nextState
      where
        nextNumbers = addNumber (turn state) x (numbers state)
        nextState = state{turn = turn state + 1, lastSpoken = x, numbers = nextNumbers}

solve :: String -> String
solve content = formatResults part1 part2
  where
    numbers = map read $ splitOn "," content
    part1 = getSpoken $ runGame 2020 $ initializeState numbers
    part2 = getSpoken $ runGame 30000000 $ initializeState numbers
