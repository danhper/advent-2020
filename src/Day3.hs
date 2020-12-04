module Day3 (
    solve,
) where

import qualified Data.Array as A
import Utils (formatIntResults)

data Cell = Empty | Tree deriving (Show, Eq)

instance Read Cell where
    readsPrec _ ('.' : xs) = [(Empty, xs)]
    readsPrec _ ('#' : xs) = [(Tree, xs)]
    readsPrec _ _ = []

newtype Grid = Grid (A.Array (Int, Int) Cell) deriving (Show)

rowsCount :: Grid -> Int
rowsCount (Grid arr) = (fst . snd) (A.bounds arr) + 1

colsCount :: Grid -> Int
colsCount (Grid arr) = (snd . snd) (A.bounds arr) + 1

get :: Grid -> Int -> Int -> Cell
get grid@(Grid arr) row col = (A.!) arr (row, col `mod` colsCount grid)

instance Read Grid where
    readsPrec _ raw = [(Grid (A.array bounds result), "")]
      where
        rows = lines raw
        bounds = ((0, 0), (length rows - 1, length (head rows) - 1))
        result = zip [0 ..] rows >>= readLine
        readLine (row, line) = zipWith (createCell row) [0 ..] line
        createCell row col char = ((row, col), read [char])

computeCells :: Grid -> Int -> Int -> [Cell]
computeCells grid stepDown stepRight = zipWith (get grid) rows cols
  where
    rows = [0, stepDown .. rowsCount grid - 1]
    cols = [0, stepRight ..]

countTrees :: Grid -> [(Int, Int)] -> [Int]
countTrees grid = map countTrees_
  where
    countTrees_ = length . filter (== Tree) . computeCells_
    computeCells_ = uncurry (computeCells grid)

solve :: String -> String
solve content = formatIntResults part1 part2
  where
    grid = read content
    [part1] = countTrees grid [(1, 3)]
    part2 = product $ countTrees grid [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
