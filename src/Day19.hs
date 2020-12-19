module Day19 (
    solve,
) where

import Control.Exception (assert)
import Control.Monad (void)
import Data.Bifunctor (first)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, lookAhead, many, manyTill, parse, sepBy1, sepEndBy1)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Text.Megaparsec.Char.Lexer as L
import Utils (formatIntResults)

data Rule = Composed [[Int]] | Terminal String deriving (Show)
type Rules = M.Map Int Rule

ruleParser :: Parsec Void String (Int, Rule)
ruleParser = (,) <$> number <* L.symbol space ":" <*> expr
  where
    expr = choice [terminalExpr, composedExpr]
    terminalExpr = Terminal <$> (char '"' *> manyTill L.charLiteral (char '"'))
    composedExpr = Composed <$> (number `sepBy1` space) `sepBy1` L.symbol space "|"
    number = L.lexeme space decimal
    space = void $ many (char ' ')

inputParser :: Parsec Void String (Rules, [String])
inputParser = (,) <$> rules <* newline <*> entries
  where
    rules = M.fromList <$> ruleParser `sepEndBy1` newline
    entries = manyTill L.charLiteral (lookAhead newline) `sepEndBy1` newline

expandRule :: Rules -> Int -> [String]
expandRule rules ruleId = fst $ doExpand ruleId (rules M.! ruleId) M.empty
  where
    doExpand _ (Terminal v) memo = ([v], memo)
    doExpand ruleId (Composed allSubrules) memo
        | ruleId `M.member` memo = (memo M.! ruleId, memo)
        | otherwise =
            let (res, newMemo) = foldl expandSequence ([], memo) allSubrules
             in (res, M.insert ruleId res newMemo)
    expandSequence (res, memo) subrules = first (res ++) $ foldl processRule ([""], memo) subrules
    processRule (derived, memo) ruleId = (res, memo)
      where
        rule = rules M.! ruleId
        (expanded, newMemo) = doExpand ruleId rule memo
        res = [x ++ y | x <- derived, y <- expanded]

solvePart1 :: Rules -> [String] -> Int
solvePart1 rules inputs = length $ filter (`S.member` patterns) inputs
  where
    patterns = S.fromList (expandRule rules 0)

solvePart2 :: Rules -> [String] -> Int
solvePart2 rules inputs = length $ filter isValid inputs
  where
    rules42 = S.fromList $ expandRule rules 42
    rules31 = S.fromList $ expandRule rules 31
    allLengths = S.map length $ S.union rules31 rules42
    rulesLength = assert (minimum allLengths == maximum allLengths) maximum allLengths
    isValid input = count42 > 1 && null remaining && count42 > count31 && count31 > 0
      where
        (count42, count31, remaining) = consume input 0 0 True
        consume list count42 count31 parseBoth
            | parseBoth && S.notMember relevant rules42 =
                consume list count42 count31 False
            | parseBoth && S.member relevant rules42 && S.member relevant rules31 =
                consume remaining (count42 + 1) (count31 + 1) True
            | parseBoth && S.member relevant rules42 =
                consume remaining (count42 + 1) 0 True
            | S.member relevant rules31 = consume remaining count42 (count31 + 1) False
            | otherwise = (count42, count31, list)
          where
            relevant = take rulesLength list
            remaining = drop rulesLength list

solve :: String -> String
solve content = formatIntResults part1 part2
  where
    Right (rules, inputs) = parse inputParser "(input)" content
    part1 = solvePart1 rules inputs
    part2 = solvePart2 rules inputs
