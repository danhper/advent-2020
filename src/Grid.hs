module Grid (
    get,
    rowsCount,
    colsCount,
    Grid,
    inRange,
    elems,
    map,
    mapi,
) where

import Prelude hiding (map)
import qualified Prelude as P (map)

import qualified Data.Array as A
import Data.Bifunctor (second)

newtype Grid a = Grid (A.Array (Int, Int) a) deriving (Show, Eq)

rowsCount :: Grid a -> Int
rowsCount (Grid arr) = (fst . snd) (A.bounds arr) + 1

colsCount :: Grid a -> Int
colsCount (Grid arr) = (snd . snd) (A.bounds arr) + 1

get :: Grid a -> Int -> Int -> a
get grid@(Grid arr) row col = (A.!) arr (row, col `mod` colsCount grid)

inRange :: Grid a -> (Int, Int) -> Bool
inRange (Grid arr) = A.inRange (A.bounds arr)

elems :: Grid a -> [a]
elems (Grid arr) = A.elems arr

instance Read a => Read (Grid a) where
    readsPrec _ raw = [(Grid (A.array bounds result), "")]
      where
        rows = lines raw
        bounds = ((0, 0), (length rows - 1, length (head rows) - 1))
        result = zip [0 ..] rows >>= readLine
        readLine (row, line) = zipWith (createCell row) [0 ..] line
        createCell row col char = ((row, col), read [char])

map :: (a -> a) -> Grid a -> Grid a
map f (Grid arr) = Grid $ A.array (A.bounds arr) $ P.map (second f) (A.assocs arr)

mapi :: (((Int, Int), a) -> a) -> Grid a -> Grid a
mapi f (Grid arr) = Grid $ A.array (A.bounds arr) $ P.map f' (A.assocs arr)
  where
    f' (i, v) = (i, f (i, v))
