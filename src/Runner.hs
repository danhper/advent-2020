module Runner (
  run,
) where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import Utils (problemInputFile)

solvers :: [String -> String]
solvers = [Day1.solve, Day2.solve, Day3.solve, Day4.solve]

run :: Int -> Maybe String -> IO String
run n sample
  | n > length solvers = return $ "Problem " ++ show n ++ " not solved"
  | otherwise = do
    content <- readFile $ problemInputFile n sample
    let solver = solvers !! (n - 1)
    return $ solver content
