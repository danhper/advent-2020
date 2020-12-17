module Day17 (
    solve,
) where

import Control.Arrow ((***))
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Tuple.Utils (dup)
import qualified Grid as G
import Utils (formatIntResults)

type Point = [Int]
data GridND = GridND {n :: Int, points :: M.Map Point Bool} deriving (Show)
newtype BoolCell = BoolCell Bool deriving (Show, Eq) -- for custom Read instance

instance Read BoolCell where
    readsPrec _ ('#' : xs) = [(BoolCell True, xs)]
    readsPrec _ ('.' : xs) = [(BoolCell False, xs)]
    readsPrec _ _ = []

parseInitialGrid :: Int -> String -> GridND
parseInitialGrid n content = GridND n $ M.fromList $ map toND elements2D
  where
    elements2D = G.assocs $ read content
    toND ((row, col), BoolCell b) = ([col, row] ++ replicate (n - 2) 0, b)

nextGrid :: GridND -> GridND
nextGrid (GridND n points) = GridND n $ M.fromList $ zip nextPoints (repeat False)
  where
    coords = M.keys points
    bounds = [(minimum *** maximum) $ dup $ map (!! n) (M.keys points) | n <- [0 .. n - 1]]
    nextPoints = sequence [[vmin - 1 .. vmax + 1] | (vmin, vmax) <- bounds]

getNeighborsCoords :: Point -> [Point]
getNeighborsCoords point = filter (/= point) $ sequence [[v - 1 .. v + 1] | v <- point]

step :: GridND -> GridND
step grid@(GridND n pts) = GridND n $ M.mapWithKey computeNextState (points $ nextGrid grid)
  where
    isActive point = M.findWithDefault False point pts
    computeNextState point _ = activeCount == 3 || isActive point && activeCount == 2
      where
        activeCount = length $ filter isActive (getNeighborsCoords point)

stepN :: Int -> GridND -> GridND
stepN times grid = foldr (const step) grid [1 .. times]

solve :: String -> String
solve content = formatIntResults part1 part2
  where
    runSimulation n = length $ M.filter id $ points $ stepN 6 (parseInitialGrid n content)
    part1 = runSimulation 3
    part2 = runSimulation 4
