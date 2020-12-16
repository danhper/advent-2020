module Day16 (
    solve,
) where

import Data.Void (Void)
import Text.Megaparsec (MonadParsec (try), Parsec, endBy1, many, parse, sepBy1, (<|>))
import Utils (formatIntResults)

import Data.List (isPrefixOf)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Megaparsec.Char (alphaNumChar, char, newline, spaceChar, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Rules = M.Map String [(Int, Int)]
type Ticket = [Int]

data Tickets = Tickets
    { rules :: Rules
    , yours :: Ticket
    , nearby :: [Ticket]
    }
    deriving (Show)

inputParser :: Parsec Void String Tickets
inputParser =
    Tickets <$> rules <* newline
        <* string "your ticket:"
        <* newline <*> ticket
        <* newline
        <* newline
        <* string "nearby tickets:"
        <* newline <*> tickets
  where
    ticket = L.decimal `sepBy1` char ','
    tickets = ticket `endBy1` newline
    rules = M.fromList <$> try rule `endBy1` newline
    rule = (,) <$> many (spaceChar <|> alphaNumChar) <* string ": " <*> ranges
    ranges = range `sepBy1` try (string " or ")
    range = (,) <$> L.decimal <* char '-' <*> L.decimal

isInRange :: Int -> (Int, Int) -> Bool
isInRange value (low, high) = value >= low && value <= high

isValueValid :: Rules -> Int -> Bool
isValueValid rules ticket = any (isInRange ticket) $ concat (M.elems rules)

isTicketValid :: Rules -> Ticket -> Bool
isTicketValid rules = all (isValueValid rules)

getColumn :: [Ticket] -> Int -> [Int]
getColumn tickets col = map (!! col) tickets

computeColumns :: Tickets -> M.Map String Int
computeColumns (Tickets rules yours nearby) = computeFinal candidates M.empty
  where
    validTickets = filter (isTicketValid rules) nearby
    columns = map (getColumn validTickets) [0 .. length rules - 1]
    isCandidate allRules column = all (\c -> any (isInRange c) allRules) column
    getCandidates rule = S.fromList $ map fst $ filter (isCandidate rule . snd) $ zip [0 ..] columns
    candidates = M.map getCandidates rules
    computeFinal cands final
        | null cands = final
        | otherwise = computeFinal newCands (M.union final determined)
      where
        determined = M.map S.findMax $ M.filter ((== 1) . length) cands
        newCands = M.map (\v -> S.difference v $ S.fromList $ M.elems determined) (M.difference cands determined)

solvePart2 :: Tickets -> Int
solvePart2 tickets = product $ map (yours tickets !!) columnIndices
  where
    columnsMapping = computeColumns tickets
    isDepartureColumn = isPrefixOf "departure"
    columnIndices = M.elems $ M.filterWithKey (const . isDepartureColumn) columnsMapping

solve :: String -> String
solve content = formatIntResults part1 part2
  where
    Right ticketsInfo = parse inputParser "(file)" content
    part1 = sum $ filter (not . isValueValid (rules ticketsInfo)) $ concat (nearby ticketsInfo)
    part2 = solvePart2 ticketsInfo
