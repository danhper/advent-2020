module Day4 (
    solve,
) where

import Data.Char (isDigit)
import Data.Either (isRight)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec
import Text.Read (readEither)
import Utils (formatResults)

type RawPassport = M.Map String String

data Height = Cm Int | In Int deriving (Show)
data EyeColor = Amb | Blu | Brn | Gry | Grn | Hzl | Oth deriving (Show)

instance Read EyeColor where
    readsPrec _ ('a' : 'm' : 'b' : xs) = [(Amb, xs)]
    readsPrec _ ('b' : 'l' : 'u' : xs) = [(Blu, xs)]
    readsPrec _ ('b' : 'r' : 'n' : xs) = [(Brn, xs)]
    readsPrec _ ('g' : 'r' : 'y' : xs) = [(Gry, xs)]
    readsPrec _ ('g' : 'r' : 'n' : xs) = [(Grn, xs)]
    readsPrec _ ('h' : 'z' : 'l' : xs) = [(Hzl, xs)]
    readsPrec _ ('o' : 't' : 'h' : xs) = [(Oth, xs)]
    readsPrec _ _ = []

data Passport = Passport
    { birthYear :: Int
    , issueYear :: Int
    , expirationYear :: Int
    , height :: Height
    , hairColor :: String
    , eyeColor :: EyeColor
    , passportID :: String
    , countryId :: Maybe String
    }
    deriving (Show)

parseInt :: Int -> Int -> String -> Either String Int
parseInt start end value = parseInt_ start end $ read value
  where
    parseInt_ start end value
        | value >= start && value <= end = Right value
        | otherwise = Left ("out of range: " ++ show value ++ " not in " ++ show start ++ "-" ++ show end)

parseYear :: Int -> Int -> String -> Either String Int
parseYear start end value
    | length value == 4 = parseInt start end value
    | otherwise = Left "invalid year"

parseHeight :: String -> Either String Height
parseHeight rawHeight
    | unit == "in" = In <$> parseInt 59 76 number
    | unit == "cm" = Cm <$> parseInt 150 193 number
    | otherwise = Left "Invalid height format"
  where
    (number, unit) = splitAt (length rawHeight - 2) rawHeight

parseHairColor :: String -> Either String String
parseHairColor rawColor
    | length rawColor == 7 && head rawColor == '#' && all isHexa (tail rawColor) = Right (tail rawColor)
    | otherwise = Left "invalid color format"
  where
    isHexa c = isDigit c || (c >= 'a' && c <= 'f')

parsePassportNumber :: String -> Either String String
parsePassportNumber rawNumber
    | length rawNumber == 9 && all isDigit rawNumber = Right rawNumber
    | otherwise = Left "invalid passport number"

parsePassport :: RawPassport -> Either String Passport
parsePassport rawPassport =
    Passport
        <$> parseValue "byr" rawPassport (parseYear 1920 2002)
        <*> parseValue "iyr" rawPassport (parseYear 2010 2020)
        <*> parseValue "eyr" rawPassport (parseYear 2020 2030)
        <*> parseValue "hgt" rawPassport parseHeight
        <*> parseValue "hcl" rawPassport parseHairColor
        <*> parseValue "ecl" rawPassport readEither
        <*> parseValue "pid" rawPassport parsePassportNumber
        <*> Right (M.lookup "cid" rawPassport)
  where
    parseValue key rawPassport parseFunc = parseValue' (M.lookup key rawPassport) parseFunc
    parseValue' Nothing _ = Left "key missing"
    parseValue' (Just value) parseFunc = parseFunc value

parseInput :: String -> [RawPassport]
parseInput content = result
  where
    Right result = parse passports "(failed)" content
    passports = sepEndBy passport (try (newline >> newline))
    passport = M.fromList <$> sepEndBy field (try fieldSep)
    field = (,) <$> many1 letter <* char ':' <*> many (alphaNum <|> char '#')
    fieldSep = (space <|> newline) >> lookAhead (noneOf "\n")

checkPassport :: RawPassport -> Bool
checkPassport passport = null (S.difference requiredKeys keys)
  where
    keys = M.keysSet passport
    requiredKeys = S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

solve :: String -> String
solve content = formatResults part1 part2
  where
    passportsList = parseInput content
    part1 = length $ filter checkPassport passportsList
    parsedPassports = map parsePassport passportsList
    part2 = length $ filter isRight parsedPassports
