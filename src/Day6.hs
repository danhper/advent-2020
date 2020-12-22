module Day6 (
  solve,
) where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Utils (formatResults)

data Group = Group
  { answers :: M.Map Char Int
  , participants :: Int
  }

parseLines :: String -> [Group]
parseLines content = map parseGroup groups
 where
  groups = splitOn [""] (lines content)
  parseGroup group =
    Group
      { answers = foldr insert M.empty $ concat group
      , participants = length group
      }
  insert c = M.insertWith (const (+ 1)) c 1

countAnswers :: Group -> Int
countAnswers group = length $ M.filter (== participants group) (answers group)

solve :: String -> String
solve content = formatResults part1 part2
 where
  groups = parseLines content
  part1 = sum $ map (M.size . answers) groups
  part2 = sum $ map countAnswers groups
