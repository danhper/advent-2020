module Utils
  ( integersFromFile,
    inputFile,
  )
where

import System.FilePath ((</>))

integersFromFile :: FilePath -> IO [Int]
integersFromFile file = map read . lines <$> readFile file

inputFile :: FilePath -> FilePath
inputFile filename = "data/inputs" </> filename
