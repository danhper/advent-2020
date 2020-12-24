module Day24 (
    solve,
) where

import qualified Data.Map as M
import Utils (formatResults)

type Grid = M.Map (Int, Int) Bool

data Direction
    = East
    | SouthEast
    | SouthWest
    | West
    | NorthWest
    | NorthEast
    deriving (Show)

directions :: [Direction]
directions = [East, SouthEast, SouthWest, West, NorthWest, NorthEast]

type Path = [Direction]

instance Read Direction where
    readsPrec _ ('n' : 'e' : xs) = [(NorthEast, xs)]
    readsPrec _ ('n' : 'w' : xs) = [(NorthWest, xs)]
    readsPrec _ ('s' : 'w' : xs) = [(SouthWest, xs)]
    readsPrec _ ('s' : 'e' : xs) = [(SouthEast, xs)]
    readsPrec _ ('w' : xs) = [(West, xs)]
    readsPrec _ ('e' : xs) = [(East, xs)]
    readsPrec _ _ = []

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) NorthEast = (x + 1, y + 1)
move (x, y) NorthWest = (x - 1, y + 1)
move (x, y) East = (x + 2, y)
move (x, y) West = (x - 2, y)
move (x, y) SouthWest = (x - 1, y - 1)
move (x, y) SouthEast = (x + 1, y - 1)

readPath :: String -> Path
readPath content = case reads content of
    [] -> []
    [(direction, remaining)] -> direction : readPath remaining

computePath :: Path -> (Int, Int)
computePath = foldl move (0, 0)

executeInstruction :: Grid -> Path -> Grid
executeInstruction grid path = M.insertWith (/=) (computePath path) True grid

executeInstructions :: Grid -> [Path] -> Grid
executeInstructions = foldl executeInstruction

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors point = map (move point) directions

step :: Grid -> Grid
step grid = M.mapWithKey updateCell $ M.union grid expansion
  where
    expansion = M.fromList $ zip newCells (repeat False)
    newCells = filter (`M.notMember` grid) $ concatMap neighbors $ M.keys grid
    updateCell point isBlack = computeColor isBlack blackCount
      where
        blackCount = length $ filter (\c -> M.findWithDefault False c grid) $ neighbors point
    computeColor isBlack blackCount
        | isBlack && blackCount == 0 || blackCount > 2 = False
        | not isBlack && blackCount == 2 = True
        | otherwise = isBlack

runSimulation :: Int -> Grid -> Grid
runSimulation 0 grid = grid
runSimulation n grid = runSimulation (n - 1) (step grid)

solve :: String -> String
solve content = formatResults part1 part2
  where
    instructions = map readPath $ lines content
    grid = executeInstructions M.empty instructions
    part1 = length $ M.filter id grid
    part2 = length $ M.filter id (runSimulation 100 grid)
