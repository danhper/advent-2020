module Grid (
    BoolCell (..),
    get,
    xMax,
    yMax,
    xMin,
    yMin,
    getCol,
    getRow,
    Grid,
    BoolGrid,
    inRange,
    grid,
    elems,
    map,
    mapi,
    assocs,
    rotate,
    flip,
    elemsInBounds,
) where

import Data.List (intercalate)
import Prelude hiding (flip, map)
import qualified Prelude as P (map)

import qualified Data.Array as A

import Control.Exception (assert)
import Data.Bifunctor (second)

newtype BoolCell = BoolCell Bool deriving (Eq) -- for custom Read instance

instance Show BoolCell where
    show (BoolCell True) = "#"
    show (BoolCell False) = "."

instance Read BoolCell where
    readsPrec _ ('#' : xs) = [(BoolCell True, xs)]
    readsPrec _ ('.' : xs) = [(BoolCell False, xs)]
    readsPrec _ _ = []

data Grid a = Grid
    { arr :: A.Array (Int, Int) a
    , rotation :: Int
    , flipped :: (Bool, Bool)
    }
    deriving (Eq)

type BoolGrid = Grid BoolCell

instance Read a => Read (Grid a) where
    readsPrec _ raw = [(Grid{arr = A.array bounds result, rotation = 0, flipped = (False, False)}, "")]
      where
        rows = lines raw
        bounds = ((0, 0), (length (head rows) - 1, length rows - 1))
        result = zip [0 ..] rows >>= readLine
        readLine (row, line) = zipWith (createCell row) [0 ..] line
        createCell row col char = ((col, row), read [char])

instance Show a => Show (Grid a) where
    show grid@Grid{arr = arr} = intercalate "\n" rows
      where
        ((xMin, yMin), (xMax, yMax)) = transformedBounds grid
        rows = P.map makeRow [yMin .. yMax]
        makeRow row = concatMap show (rowElems row)
        rowElems row = A.elems $ A.ixmap bounds (`transformCoords` grid) arr
          where
            bounds = ((xMin, row), (xMax, row))

xMin :: Grid a -> Int
xMin grid = (fst . fst) (transformedBounds grid)

yMin :: Grid a -> Int
yMin grid = (snd . fst) (transformedBounds grid)

xMax :: Grid a -> Int
xMax grid = (fst . snd) (transformedBounds grid)

yMax :: Grid a -> Int
yMax grid = (snd . snd) (transformedBounds grid)

transformBounds :: ((Int, Int), (Int, Int)) -> Grid a -> ((Int, Int), (Int, Int))
transformBounds bounds Grid{arr = arr, rotation = rotation} =
    if rotation `elem` [90, 270] then ((yMin, xMin), (yMax, xMax)) else bounds
  where
    ((xMin, yMin), (xMax, yMax)) = bounds

transformedBounds :: Grid a -> ((Int, Int), (Int, Int))
transformedBounds grid@Grid{arr = arr} = transformBounds (A.bounds arr) grid

transformCoords :: (Int, Int) -> Grid a -> (Int, Int)
transformCoords (x, y) grid@Grid{arr = arr, rotation = rotation, flipped = (xFlip, yFlip)} =
    (makeXFlip xFlip . makeYFlip yFlip . makeRotation rotation) (x, y)
  where
    ((xMin, yMin), (xMax, yMax)) = transformedBounds grid
    makeXFlip False (x, y) = (x, y)
    makeXFlip True (x, y) = (xMax - x + xMin, y)
    makeYFlip False (x, y) = (x, y)
    makeYFlip True (x, y) = (x, yMax - y + yMin)
    makeRotation 0 (x, y) = (x, y)
    makeRotation 90 (x, y) = (yMax - y + yMin, x)
    makeRotation 180 (x, y) = (xMax - x + xMin, yMax - y + yMin)
    makeRotation 270 (x, y) = (y, xMax - x + xMin)

flip :: (Bool, Bool) -> Grid a -> Grid a
flip flipped grid = grid{flipped = flipped}

rotate :: Int -> Grid a -> Grid a
rotate delta grid@Grid{rotation = rotation} =
    assert (newRot `mod` 90 == 0) grid{rotation = newRot}
  where
    newRot = rotation + delta `mod` 360

get :: Int -> Int -> Grid a -> a
get x y grid@Grid{arr = arr} = (A.!) arr (x', y')
  where
    (x', y') = transformCoords (x, y) grid

inRange :: Grid a -> (Int, Int) -> Bool
inRange Grid{arr = arr} = A.inRange (A.bounds arr)

elems :: Grid a -> [a]
elems Grid{arr = arr} = A.elems arr

assocs :: Grid a -> [((Int, Int), a)]
assocs Grid{arr = arr} = A.assocs arr

map :: (a -> a) -> Grid a -> Grid a
map f grid@Grid{arr = arr} = grid{arr = A.array (A.bounds arr) $ P.map (second f) (A.assocs arr)}

mapi :: (((Int, Int), a) -> a) -> Grid a -> Grid a
mapi f grid@Grid{arr = arr} = grid{arr = A.array (A.bounds arr) $ P.map f' (A.assocs arr)}
  where
    f' (i, v) = (i, f (i, v))

elemsInBounds :: ((Int, Int), (Int, Int)) -> Grid a -> [a]
elemsInBounds bounds grid@Grid{arr = arr} =
    A.elems $ A.ixmap bounds (`transformCoords` grid) arr

getCol :: Int -> Grid a -> [a]
getCol col grid = elemsInBounds ((col', yMin grid), (col', yMax grid)) grid
  where
    col' = col `mod` (xMax grid + 1)

getRow :: Int -> Grid a -> [a]
getRow row grid = elemsInBounds ((xMin grid, row'), (xMax grid, row')) grid
  where
    row' = row `mod` (yMax grid + 1)

grid :: ((Int, Int), (Int, Int)) -> [((Int, Int), a)] -> Grid a
grid bounds elems = Grid{arr = arr, rotation = 0, flipped = (False, False)}
  where
    arr = A.array bounds elems
