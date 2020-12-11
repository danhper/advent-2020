module Day11 (
    solve,
) where

import Utils (
    fixedPoint,
    formatIntResults,
 )

import qualified Grid as G

data Cell = Floor | Occupied | Empty deriving (Show, Eq)

instance Read Cell where
    readsPrec _ ('.' : xs) = [(Floor, xs)]
    readsPrec _ ('L' : xs) = [(Empty, xs)]
    readsPrec _ ('#' : xs) = [(Occupied, xs)]
    readsPrec _ _ = []

firstIsOccupied :: [Cell] -> Bool
firstIsOccupied cells = case filter (/= Floor) cells of
    (Occupied : _) -> True
    _ -> False

countAdjacentOccupied :: G.Grid Cell -> (Int, Int) -> Int
countAdjacentOccupied grid (row, col) = length $ filter (== Occupied) adjacentSeatValues
  where
    candidates = [(r, c) | r <- [row - 1 .. row + 1], c <- [col - 1 .. col + 1], not (row == r && col == c)]
    adjacentSeats = filter (G.inRange grid) candidates
    adjacentSeatValues = map (uncurry $ G.get grid) adjacentSeats

countInSightOccuppied :: G.Grid Cell -> (Int, Int) -> Int
countInSightOccuppied grid (row, col) = length $ filter firstIsOccupied cells
  where
    topRows = tail [row, row - 1 .. 0]
    bottomRows = tail [row, row + 1 .. G.rowsCount grid - 1]
    leftCols = tail [col, col - 1 .. 0]
    rightCols = tail [col, col + 1 .. G.colsCount grid - 1]
    positions =
        [ zip topRows (repeat col) -- top
        , zip bottomRows (repeat col) -- bottom
        , zip (repeat row) leftCols -- left
        , zip (repeat row) rightCols -- right
        , zip topRows leftCols -- top left
        , zip topRows rightCols -- top right
        , zip bottomRows leftCols -- bottom left
        , zip bottomRows rightCols -- bottom right
        ]
    cells = map (map (uncurry $ G.get grid)) positions

applyRule :: (G.Grid Cell -> (Int, Int) -> Int) -> Int -> G.Grid Cell -> ((Int, Int), Cell) -> Cell
applyRule countOccupied minOccupied grid (position, cell) = case (countOccupied grid position, cell) of
    (0, Empty) -> Occupied
    (v, Occupied)
        | v >= minOccupied -> Empty
        | otherwise -> cell
    _ -> cell

runSimulationStep :: (G.Grid Cell -> (Int, Int) -> Int) -> Int -> G.Grid Cell -> G.Grid Cell
runSimulationStep countOccupied minOccupied grid =
    G.mapi (applyRule countOccupied minOccupied grid) grid

runSimulation :: (G.Grid Cell -> (Int, Int) -> Int) -> Int -> G.Grid Cell -> G.Grid Cell
runSimulation countOccupied minOccupied = fixedPoint (runSimulationStep countOccupied minOccupied)

getOccupiedCount :: G.Grid Cell -> Int
getOccupiedCount grid = length $ filter (== Occupied) (G.elems grid)

solve :: String -> String
solve content = formatIntResults part1 part2
  where
    grid = read content
    part1 = getOccupiedCount $ runSimulation countAdjacentOccupied 4 grid
    part2 = getOccupiedCount $ runSimulation countInSightOccuppied 5 grid
