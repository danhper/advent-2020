module Runner
  ( run,
  )
where

import qualified Day1
import qualified Day2
import qualified Day3
import Utils (problemInputFile)

solvers :: [String -> String]
solvers = [Day1.solve, Day2.solve, Day3.solve]

run :: Int -> Bool -> IO String
run n isSample
  | n > length solvers = return $ "Problem " ++ show n ++ " not solved"
  | otherwise = do
    content <- readFile $ problemInputFile n isSample
    let solver = solvers !! (n - 1)
    return $ solver content
