module Utils (
  parseIntegersList,
  inputFile,
  problemInputFile,
  formatIntResult,
  formatIntResults,
  readInt,
  fixedPoint
) where

import System.FilePath ((</>))
import Text.Read (readMaybe)

parseIntegersList :: String -> [Int]
parseIntegersList = map read . lines

inputFile :: FilePath -> FilePath
inputFile filename = "data/inputs" </> filename

problemInputFile :: Int -> Maybe String -> FilePath
problemInputFile n sample = inputFile $ "day" ++ show n ++ suffix ++ ".txt"
 where
  suffix = case sample of
    Nothing -> ""
    Just v -> "-sample" ++ v

formatIntResult :: String -> Int -> String
formatIntResult prefix value = prefix ++ ": " ++ show value ++ "\n"

formatIntResults :: Int -> Int -> String
formatIntResults a b = formatIntResult "Part 1" a ++ formatIntResult "Part 2" b

readInt :: String -> Maybe Int
readInt ('+' : value) = readMaybe value
readInt value = readMaybe value

fixedPoint :: Eq t => (t -> t) -> t -> t
fixedPoint f x = let x' = f x in if x == x' then x else fixedPoint f x'
