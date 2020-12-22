module Utils (
    parseIntegersList,
    inputFile,
    problemInputFile,
    formatResult,
    formatResults,
    readSignedInt,
    fixedPoint,
    readBin,
    showBin,
    showPaddedBin,
) where

import Data.Char (digitToInt)
import Numeric (readInt, showIntAtBase)
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

formatResult :: (Show a) => String -> a -> String
formatResult prefix value = prefix ++ ": " ++ show value ++ "\n"

formatResults :: (Show a, Show b) => a -> b -> String
formatResults a b = formatResult "Part 1" a ++ formatResult "Part 2" b

readSignedInt :: String -> Maybe Int
readSignedInt ('+' : value) = readMaybe value
readSignedInt value = readMaybe value

fixedPoint :: Eq t => (t -> t) -> t -> t
fixedPoint f x = let x' = f x in if x == x' then x else fixedPoint f x'

readBin :: String -> Int
readBin binstr = case readInt 2 (`elem` "01") digitToInt binstr of
    [(v, "")] -> v
    _ -> error "could not parse binary string"

showBin :: Int -> String
showBin value = showIntAtBase 2 ("01" !!) value ""

showPaddedBin :: Int -> Int -> String
showPaddedBin minLength value = replicate (minLength - length res) '0' ++ res
  where
    res = showBin value
