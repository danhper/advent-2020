module Day17 (
    solve,
) where

import qualified Data.Set as S
import qualified Grid as G
import Utils (formatIntResults)

type Point = [Int]

parseInitialGrid :: Int -> String -> S.Set Point
parseInitialGrid n content = S.fromList $ map toND elements2D
  where
    elements2D = filter (\(_, G.BoolCell b) -> b) $ G.assocs $ read content
    toND ((row, col), _) = [col, row] ++ replicate (n - 2) 0

nextGrid :: S.Set Point -> S.Set Point
nextGrid points = S.filter nextActive nextPoints
  where
    nextPoints = S.union points $ S.fromList (concatMap getNeighborsCoords points)
    nextActive point = activeCount == 3 || S.member point points && activeCount == 2
      where
        activeCount = length $ filter (`S.member` points) (getNeighborsCoords point)

getNeighborsCoords :: Point -> [Point]
getNeighborsCoords point = filter (/= point) $ sequence [[v - 1 .. v + 1] | v <- point]

stepNS :: Int -> S.Set Point -> S.Set Point
stepNS times grid = foldr (const nextGrid) grid [1 .. times]

solve :: String -> String
solve content = formatIntResults part1 part2
  where
    runSimulationS n = length $ stepNS 6 (parseInitialGrid n content)
    part1 = runSimulationS 3
    part2 = runSimulationS 4
