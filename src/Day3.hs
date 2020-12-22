module Day3 (
    solve,
) where

import qualified Grid as G
import Utils (formatResults)

data Cell = Empty | Tree deriving (Show, Eq)

instance Read Cell where
    readsPrec _ ('.' : xs) = [(Empty, xs)]
    readsPrec _ ('#' : xs) = [(Tree, xs)]
    readsPrec _ _ = []

computeCells :: G.Grid Cell -> Int -> Int -> [Cell]
computeCells grid stepDown stepRight = zipWith getCell xs ys
  where
    ys = [0, stepDown .. G.yMax grid]
    xs = [0, stepRight ..]
    getCell x y = G.get (x `mod` (G.xMax grid + 1)) y grid

countTrees :: G.Grid Cell -> [(Int, Int)] -> [Int]
countTrees grid = map countTrees_
  where
    countTrees_ = length . filter (== Tree) . computeCells_
    computeCells_ = uncurry (computeCells grid)

solve :: String -> String
solve content = formatResults part1 part2
  where
    grid = read content
    [part1] = countTrees grid [(1, 3)]
    part2 = product $ countTrees grid [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
