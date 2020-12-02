{-# LANGUAGE OverloadedStrings #-}

module Day2
  ( run,
  )
where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.String.Utils (strip)
import qualified Data.Text as T
import Formatting (formatToString, int, (%))
import Utils (inputFile)

type CharCount = M.Map Char Int

data Rule = Rule
  { letter :: Char,
    left :: Int,
    right :: Int
  }

data PasswordLine = PasswordLine
  { password :: T.Text,
    charCount :: CharCount,
    rule :: Rule
  }

countChars :: T.Text -> CharCount
countChars = T.foldr insert M.empty
  where
    insert c = M.insertWith addChar c 1
    addChar _ count = count + 1

checkRule1 :: PasswordLine -> Bool
checkRule1 PasswordLine {charCount = charCount, rule = rule} =
  occurences >= left rule && occurences <= right rule
  where
    occurences = fromMaybe 0 $ M.lookup (letter rule) charCount

checkRule2 :: PasswordLine -> Bool
checkRule2 PasswordLine {password = password, rule = rule} = xor leftMatch rightMatch
  where
    Rule {letter = letter, left = left, right = right} = rule
    leftMatch = T.index password (left - 1) == letter
    rightMatch = T.index password (right - 1) == letter
    xor a b = (a || b) && not (a && b)

parseRule :: String -> Rule
parseRule rawRule = Rule {letter = letter, left = left, right = right}
  where
    [occurences, [letter]] = splitOn " " rawRule
    [left, right] = map read (splitOn "-" occurences)

parseLine :: String -> PasswordLine
parseLine line = PasswordLine {password = password, rule = rule, charCount = charCount}
  where
    [rawRule, rawPassword] = splitOn ":" line
    password = T.pack (strip rawPassword)
    rule = parseRule (strip rawRule)
    charCount = countChars password

solveDay2 :: [String] -> (Int, Int)
solveDay2 rawPasswords = (answer1, answer2)
  where
    parsedPasswords = map parseLine rawPasswords
    answer1 = length $ filter checkRule1 parsedPasswords
    answer2 = length $ filter checkRule2 parsedPasswords

run :: IO String
run = do
  passwords <- lines <$> readFile (inputFile "day2.txt")
  let (part1, part2) = solveDay2 passwords
  return $ formatToString ("Part 1: " % int % "\nPart 2: " % int % "\n") part1 part2
