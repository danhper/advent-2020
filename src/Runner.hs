module Runner (
  run,
) where

import qualified Day1
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import Utils (problemInputFile)

solvers :: [String -> String]
solvers =
  [ Day1.solve
  , Day2.solve
  , Day3.solve
  , Day4.solve
  , Day5.solve
  , Day6.solve
  , Day7.solve
  , Day8.solve
  , Day9.solve
  , Day10.solve
  , Day11.solve
  , Day12.solve
  ]

run :: Int -> Maybe String -> IO String
run n sample
  | n > length solvers = return $ "Problem " ++ show n ++ " not solved"
  | otherwise = do
    content <- readFile $ problemInputFile n sample
    let solver = solvers !! (n - 1)
    return $ solver content
