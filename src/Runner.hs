module Runner (
    run,
) where

import qualified Day1
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day2
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
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
    , Day13.solve
    , Day14.solve
    , Day15.solve
    , Day16.solve
    , Day17.solve
    , Day18.solve
    , Day19.solve
    , Day20.solve
    , Day21.solve
    , Day22.solve
    , Day23.solve
    , Day24.solve
    ]

run :: Int -> Maybe String -> IO String
run n sample
    | n > length solvers = return $ "Problem " ++ show n ++ " not solved"
    | otherwise = do
        content <- readFile $ problemInputFile n sample
        let solver = solvers !! (n - 1)
        return $ solver content
