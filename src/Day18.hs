module Day18 (
    solve,
) where

import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.Either (rights)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (try), Parsec, between, parse, (<|>))
import Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as L
import Utils (formatResults)

data Expr = Add Expr Expr | Mul Expr Expr | Const Int deriving (Show, Eq)

type ParseTable = [[Operator (Parsec Void String) Expr]]

evalExpr :: Expr -> Int
evalExpr (Add left right) = evalExpr left + evalExpr right
evalExpr (Mul left right) = evalExpr left * evalExpr right
evalExpr (Const v) = v

symbol :: String -> Parsec Void String String
symbol = L.symbol space

part1Table :: ParseTable
part1Table = [[InfixL (Add <$ symbol "+"), InfixL (Mul <$ symbol "*")]]

part2Table :: ParseTable
part2Table = [[InfixL (Add <$ symbol "+")], [InfixL (Mul <$ symbol "*")]]

exprParser :: ParseTable -> Parsec Void String Expr
exprParser table = makeExprParser term table
  where
    term = parens (exprParser table) <|> number
    parens = between (symbol "(") (symbol ")")
    number = Const <$> L.lexeme space L.decimal

solve :: String -> String
solve content = formatResults part1 part2
  where
    parseExprs table = rights $ map (parse (exprParser table) "(file)") $ lines content
    part1 = sum $ map evalExpr (parseExprs part1Table)
    part2 = sum $ map evalExpr (parseExprs part2Table)
