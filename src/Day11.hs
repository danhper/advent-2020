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
countAdjacentOccupied grid (x, y) = length $ filter (== Occupied) adjacentSeatValues
  where
    candidates = [(x', y') | x' <- [x - 1 .. x + 1], y' <- [y - 1 .. y + 1], not (x == x' && y == y')]
    adjacentSeats = filter (G.inRange grid) candidates
    adjacentSeatValues = map (\(x, y) -> G.get x y grid) adjacentSeats

countInSightOccuppied :: G.Grid Cell -> (Int, Int) -> Int
countInSightOccuppied grid (x, y) = length $ filter firstIsOccupied cells
  where
    topRows = tail [y, y - 1 .. 0]
    bottomRows = tail [y, y + 1 .. G.yMax grid]
    leftCols = tail [x, x - 1 .. 0]
    rightCols = tail [x, x + 1 .. G.xMax grid]
    positions =
        [ zip (repeat x) topRows -- top
        , zip (repeat x) bottomRows -- bottom
        , zip leftCols (repeat y) -- left
        , zip rightCols (repeat y) -- right
        , zip leftCols topRows -- top left
        , zip rightCols topRows -- top right
        , zip leftCols bottomRows -- bottom left
        , zip rightCols bottomRows -- bottom right
        ]
    cells = map (map (\(x, y) -> G.get x y grid)) positions

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
