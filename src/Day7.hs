module Day7 (
    solve,
) where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Utils (formatIntResults)

type Rules = M.Map String (M.Map String Int)

parseRule :: String -> (String, M.Map String Int)
parseRule rawRule = (unwords (take 2 tokens), M.fromList (parseSubRules rawRules))
  where
    tokens = words rawRule
    rawRules = map words $ splitOn ", " $ unwords $ drop 4 tokens
    parseSubRules [] = []
    parseSubRules ["no" : _] = []
    parseSubRules ((number : color1 : color2 : _) : xs) = (unwords [color1, color2], read number) : parseSubRules xs

expandRules :: Rules -> Rules
expandRules rules = M.map computeNewRules rules
  where
    findRule key = M.findWithDefault M.empty key rules
    joinRules (key, value) = M.map (* value) $ computeNewRules $ findRule key
    computeNewRules subRules = M.unionsWith (+) (subRules : map joinRules (M.toList subRules))

solve :: String -> String
solve content = formatIntResults part1 part2
  where
    rules = expandRules . M.fromList . map parseRule . lines $ content
    part1 = length $ M.filter (M.member "shiny gold") rules
    part2 = M.foldl (+) 0 (rules M.! "shiny gold")
