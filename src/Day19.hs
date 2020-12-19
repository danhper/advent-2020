module Day19 (
    solve,
) where

import Control.Monad (void)
import Data.List (concatMap)
import qualified Data.Map as M
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, lookAhead, many, manyTill, parse, sepBy1, sepEndBy1)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Text.Megaparsec.Char.Lexer as L
import Utils (formatIntResults)

data Rule = Composed [[Int]] | Terminal String deriving (Show)
type Rules = M.Map Int Rule

inputParser :: Parsec Void String (Rules, [String])
inputParser = (,) <$> rules <* newline <*> entries
  where
    rules = M.fromList <$> ruleParser `sepEndBy1` newline
    entries = manyTill L.charLiteral (lookAhead newline) `sepEndBy1` newline
    ruleParser = (,) <$> number <* L.symbol space ":" <*> choice [terminalExpr, composedExpr]
    terminalExpr = Terminal <$> (char '"' *> manyTill L.charLiteral (char '"'))
    composedExpr = Composed <$> (number `sepBy1` space) `sepBy1` L.symbol space "|"
    number = L.lexeme space decimal
    space = void $ many (char ' ')

acceptsInput :: Rules -> String -> Bool
acceptsInput rules input = any (\(ok, remaining) -> ok && null remaining) results
  where
    results = acceptsInput' input $ rules M.! 0
    acceptsInput' [] _ = [(False, "")]
    acceptsInput' (x : xs) (Terminal v)
        | v == [x] = [(True, xs)]
        | otherwise = [(False, input)]
    acceptsInput' input (Composed subrules) = concatMap (acceptBranch input) subrules
    acceptBranch input [] = [(True, input)]
    acceptBranch input (r : rs) = concatMap processResults results
      where
        results = acceptsInput' input (rules M.! r)
        processResults (True, remaining) = acceptBranch remaining rs
        processResults (False, remaining) = [(False, remaining)]

solve :: String -> String
solve content = formatIntResults part1 part2
  where
    Right (rules, inputs) = parse inputParser "(input)" content
    part1 = length $ filter (acceptsInput rules) inputs
    rulesPart2 = M.insert 8 (Composed [[42], [42, 8]]) $ M.insert 11 (Composed [[42, 31], [42, 11, 31]]) rules
    part2 = length $ filter (acceptsInput rulesPart2) inputs
