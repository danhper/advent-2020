{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( parseIntegersList,
    inputFile,
    problemInputFile,
    formatIntResult,
    formatIntResults,
  )
where

import System.FilePath ((</>))

parseIntegersList :: String -> [Int]
parseIntegersList = map read . lines

inputFile :: FilePath -> FilePath
inputFile filename = "data/inputs" </> filename

problemInputFile :: Int -> Bool -> FilePath
problemInputFile n isSample = inputFile $ "day" ++ show n ++ suffix ++ ".txt"
  where
    suffix = if isSample then "-sample" else ""

formatIntResult :: String -> Int -> String
formatIntResult prefix value = prefix ++ ": " ++ show value ++ "\n"

formatIntResults :: Int -> Int -> String
formatIntResults a b = formatIntResult "Part 1" a ++ formatIntResult "Part 2" b
