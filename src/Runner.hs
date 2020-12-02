module Runner
  ( run,
  )
where

import qualified Day1
import qualified Day2

run :: Int -> IO String
run 1 = Day1.run
run 2 = Day2.run
