module Day20 (
    solve,
) where

import Control.Applicative (Alternative ((<|>)))
import Data.List (find)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Debug.Trace (trace)
import qualified Grid as G
import Utils (formatIntResults)

type CameraArray = M.Map Int G.BoolGrid

data Arrangement = Arrangement
    { grids :: M.Map Int (Int, G.BoolGrid)
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

tryArrangement :: Arrangement -> Int -> G.BoolGrid -> Maybe Arrangement
tryArrangement arrangement@Arrangement{grids = grids, size = size} i grid =
    if leftFits && upperFits then Just arrangement{grids = M.insert nextIndex (i, grid) grids} else Nothing
  where
    nextIndex = trace (show $ M.size grids) M.size grids
    upperGrid = snd <$> grids M.!? (nextIndex - size)
    leftGrid = if nextIndex `mod` size == 0 then Nothing else Just $ snd $ grids M.! (nextIndex - 1)
    leftFits = maybe True (\left -> G.getCol 0 grid == G.getCol (-1) left) leftGrid
    upperFits = maybe True (\upper -> G.getRow 0 grid == G.getRow (-1) upper) upperGrid

findArrangement :: CameraArray -> Arrangement -> Maybe Arrangement
findArrangement camArray arrangement
    | M.null camArray = Just arrangement
    | otherwise = listToMaybe $ concatMap getValidArrangements (M.assocs camArray)
  where
    getValidArrangements (i, grid) = mapMaybe (findArrangement (M.delete i camArray)) validNexts
      where
        transformations = computeTransformations grid
        validNexts = trace ("i: " ++ show i ++ ", tr: " ++ show (length transformations)) $ mapMaybe (tryArrangement arrangement i) transformations

solvePart1 :: CameraArray -> [Int]
solvePart1 camArray = map (fst . (final M.!)) points
  where
    initial = emptyArrangement camArray
    Just (Arrangement final size) = findArrangement camArray initial
    points = [0, size - 1, size * (size - 1), size * size - 1]

solve :: String -> String
solve content = formatIntResults part1 part2
  where
    cameraArray = parseArray content
    part1 = product $ solvePart1 cameraArray
    part2 = 0
