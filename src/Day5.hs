module Day5 (
    solve,
) where

import Data.List (find)
import Data.List.Utils (replace)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Utils (formatIntResults)

parseBinary :: String -> Int
parseBinary binaryString = parseBinary' (reverse binaryString) 1
  where
    parseBinary' [] _ = 0
    parseBinary' (x : xs) multiplier = read [x] * multiplier + parseBinary' xs (multiplier * 2)

parseTicketNumber :: String -> (Int, Int)
parseTicketNumber ticketNumber = (parseBinary row, parseBinary col)
  where
    (rawRow, rawCol) = splitAt 7 ticketNumber
    row = replace "F" "0" $ replace "B" "1" rawRow
    col = replace "L" "0" $ replace "R" "1" rawCol

ticketId :: String -> Int
ticketId = (\(row, col) -> row * 8 + col) . parseTicketNumber

findSeat :: S.Set Int -> Int
findSeat occupiedSeats = fromJust (find isFree freeSeats)
  where
    totalSeats = 2 ^ 7 * 2 ^ 3
    allSeats = S.fromList [0 .. totalSeats - 1]
    freeSeats = S.difference allSeats occupiedSeats
    isFree seat = S.member (seat + 1) occupiedSeats && S.member (seat - 1) occupiedSeats

solve :: String -> String
solve content = formatIntResults part1 part2
  where
    ticketNumbers = lines content
    ticketIds = map ticketId ticketNumbers
    part1 = maximum ticketIds
    part2 = findSeat (S.fromList ticketIds)
