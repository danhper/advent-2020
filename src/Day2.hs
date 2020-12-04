module Day2 (
    solve,
) where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.String.Utils (strip)
import qualified Data.Text as T
import Utils (formatIntResults)

type CharCount = M.Map Char Int

data Rule = Rule
    { letter :: Char
    , left :: Int
    , right :: Int
    }

instance Read Rule where
    readsPrec _ rawRule =
        [(Rule{letter = letter, left = left, right = right}, "")]
      where
        [occurences, [letter]] = splitOn " " rawRule
        [left, right] = map read (splitOn "-" occurences)

data PasswordLine = PasswordLine
    { password :: T.Text
    , charCount :: CharCount
    , rule :: Rule
    }

instance Read PasswordLine where
    readsPrec _ line = [(passwordLine, "")]
      where
        passwordLine =
            PasswordLine{password = password, rule = rule, charCount = charCount}
        [rawRule, rawPassword] = splitOn ":" line
        password = T.pack (strip rawPassword)
        rule = read (strip rawRule)
        charCount = countChars password

countChars :: T.Text -> CharCount
countChars = T.foldr insert M.empty
  where
    insert c = M.insertWith addChar c 1
    addChar _ count = count + 1

checkRule1 :: PasswordLine -> Bool
checkRule1 PasswordLine{charCount = charCount, rule = rule} =
    occurences >= left rule && occurences <= right rule
  where
    occurences = fromMaybe 0 $ M.lookup (letter rule) charCount

checkRule2 :: PasswordLine -> Bool
checkRule2 PasswordLine{password = password, rule = rule} =
    xor
        leftMatch
        rightMatch
  where
    Rule{letter = letter, left = left, right = right} = rule
    leftMatch = T.index password (left - 1) == letter
    rightMatch = T.index password (right - 1) == letter
    xor a b = (a || b) && not (a && b)

solveDay2 :: [PasswordLine] -> (Int, Int)
solveDay2 passwords = (count checkRule1, count checkRule2)
  where
    count rule = length $ filter rule passwords

solve :: String -> String
solve content = uncurry formatIntResults . solveDay2 $ map read (lines content)
