module Day20 (
    solve,
) where

import Control.Applicative (Alternative ((<|>)))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (maximumBy)
import Data.List (find)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S
import Data.Tuple (swap)
import Debug.Trace (trace)
import qualified Grid as G
import Utils (formatResults)

type CameraArray = M.Map Int G.BoolGrid

data Arrangement = Arrangement
    { grids :: M.Map (Int, Int) (Int, G.BoolGrid)
    , size :: Int
    }
    deriving (Show)

emptyArrangement :: CameraArray -> Arrangement
emptyArrangement cameraArray = Arrangement M.empty (round $ sqrt $ fromIntegral $ M.size cameraArray)

parseArray :: String -> CameraArray
parseArray content = M.fromList $ map parseSingle $ splitOn [""] $ lines content
  where
    parseSingle arrayLines = (parseIdLine $ head arrayLines, read $ unlines $ tail arrayLines)
    parseIdLine line = let [(v, _)] = reads $ drop (length "Tile ") line in v

computeTransformations :: G.BoolGrid -> [G.BoolGrid]
computeTransformations grid =
    [G.rotate r $ G.flip (xf, yf) grid | r <- [0, 90, 180, 270], xf <- [False, True], yf <- [False, True]]

fitsLeft :: G.BoolGrid -> G.BoolGrid -> Bool
fitsLeft a b = G.getCol 0 a == G.getCol (-1) b

fitsRight :: G.BoolGrid -> G.BoolGrid -> Bool
fitsRight a b = G.getCol (-1) a == G.getCol 0 b

fitsTop :: G.BoolGrid -> G.BoolGrid -> Bool
fitsTop a b = G.getRow 0 a == G.getRow (-1) b

fitsBottom :: G.BoolGrid -> G.BoolGrid -> Bool
fitsBottom a b = G.getRow (-1) a == G.getRow 0 b

tryArrangement :: Arrangement -> Int -> G.BoolGrid -> Maybe Arrangement
tryArrangement arrangement@Arrangement{grids = grids, size = size} i grid =
    if leftFits && upperFits then Just arrangement{grids = M.insert next (i, grid) grids} else Nothing
  where
    next@(nextX, nextY) = (M.size grids `mod` size, M.size grids `div` size)
    upperGrid = snd <$> grids M.!? (nextX, nextY - 1)
    leftGrid = snd <$> grids M.!? (nextX - 1, nextY)
    leftFits = maybe True (fitsLeft grid) leftGrid
    upperFits = maybe True (fitsTop grid) upperGrid

findArrangement :: M.Map Int Int -> CameraArray -> Arrangement -> Maybe Arrangement
findArrangement counts camArray arrangement@Arrangement{grids = grids, size = size}
    | M.null camArray = Just arrangement
    | otherwise = listToMaybe $ concatMap getValidArrangements (M.assocs candidates)
  where
    (x, y) = (M.size grids `mod` size, M.size grids `div` size)
    withCount n = M.filterWithKey (\k _ -> counts M.! k == n) camArray
    candidates
        | (x == 0 || x == size - 1) && (y == 0 || y == size - 1) = withCount 2
        | x == 0 || x == size - 1 || y == 0 || y == size - 1 = withCount 3
        | otherwise = withCount 4
    getValidArrangements (i, grid) = mapMaybe (findArrangement counts (M.delete i camArray)) validNexts
      where
        transformations = computeTransformations grid
        validNexts = mapMaybe (tryArrangement arrangement i) transformations

countMatches :: CameraArray -> M.Map Int Int
countMatches camArray = M.mapWithKey countGridMatches camArray
  where
    countGridMatches i grid = maximum $ map doCount $ computeTransformations grid
      where
        fitFuncs = [fitsLeft, fitsRight, fitsTop, fitsBottom]
        doCount g = length $ filter id [any (f g) candidates | f <- fitFuncs]
        candidates = concatMap computeTransformations $ M.elems $ M.delete i camArray

computeArrangement :: CameraArray -> Arrangement
computeArrangement camArray = fromJust $ findArrangement counts camArray initial
  where
    initial = emptyArrangement camArray
    counts = countMatches camArray

solvePart1 :: Arrangement -> [Int]
solvePart1 Arrangement{grids = final, size = size} = map (fst . (final M.!)) points
  where
    points = [(0, 0), (0, size - 1), (size - 1, 0), (size - 1, size - 1)]

stripGrid :: G.BoolGrid -> G.BoolGrid
stripGrid grid = G.grid bounds [((x - 1, y - 1), G.get x y grid) | x <- [min .. max], y <- [min .. max]]
  where
    (min, max) = (G.xMin grid + 1, G.xMax grid - 1)
    bounds = ((0, 0), (max - 1, max - 1))

combineGrids :: Arrangement -> G.BoolGrid
combineGrids arrangement@Arrangement{grids = grids, size = size} = G.grid bounds elems
  where
    strippedGrids = M.map (stripGrid . snd) grids
    firstGrid = strippedGrids M.! (0, 0)
    gridSize = G.xMax firstGrid + 1
    get x y = G.get (x `mod` gridSize) (y `mod` gridSize) $ strippedGrids M.! (x `div` gridSize, y `div` gridSize)
    fullGridSize = gridSize * size - 1
    elems = [((x, y), get x y) | x <- [0 .. fullGridSize], y <- [0 .. fullGridSize]]
    bounds = ((0, 0), (fullGridSize, fullGridSize))

--                   #
-- #    ##    ##    ###
--  #  #  #  #  #  #
seaDragonCoords :: [(Int, Int)]
seaDragonCoords =
    [ (0, 1)
    , (1, 2)
    , (4, 2)
    , (5, 1)
    , (6, 1)
    , (7, 2)
    , (10, 2)
    , (11, 1)
    , (12, 1)
    , (13, 2)
    , (16, 2)
    , (17, 1)
    , (18, 0)
    , (18, 1)
    , (19, 1)
    ]

seaDragonSize :: (Int, Int)
seaDragonSize = (19, 2)

makeSeaDragonPoints :: (Int, Int) -> [(Int, Int)]
makeSeaDragonPoints (x, y) = map (bimap (+ x) (+ y)) seaDragonCoords

seaDragonPoints :: (Int, Int) -> G.BoolGrid -> S.Set (Int, Int)
seaDragonPoints (x, y) grid =
    if inRange && isDragon then S.fromList points else S.empty
  where
    inRange = (x + fst seaDragonSize) <= G.xMax grid && (y + snd seaDragonSize) <= G.yMax grid
    isDragon = all (\(x', y') -> G.get x' y' grid == G.BoolCell True) points
    points = makeSeaDragonPoints (x, y)

findDragonCells :: G.BoolGrid -> S.Set (Int, Int)
findDragonCells grid = (fromJust . find (not . S.null) . map findDragonCells' . computeTransformations) grid
  where
    points = [(x, y) | x <- [G.xMin grid .. G.xMax grid], y <- [G.yMin grid .. G.yMax grid]]
    findDragonCells' grid = S.unions $ map (`seaDragonPoints` grid) points

solvePart2 :: Arrangement -> Int
solvePart2 arrangement = totalCells - dragonCellsCount
  where
    grid = combineGrids arrangement
    dragonCellsCount = S.size $ findDragonCells grid
    points = [(x, y) | x <- [G.xMin grid .. G.xMax grid], y <- [G.yMin grid .. G.yMax grid]]
    totalCells = length $ filter (\(x, y) -> G.get x y grid == G.BoolCell True) points

solve :: String -> String
solve content = formatResults part1 part2
  where
    cameraArray = parseArray content
    arrangement = computeArrangement cameraArray
    part1 = product $ solvePart1 arrangement
    part2 = solvePart2 arrangement
