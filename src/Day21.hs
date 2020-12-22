module Day21 (
    solve,
) where

import qualified Data.Set as S
import Utils (fixedPoint, formatResults)

import Control.Applicative (Alternative ((<|>)))
import Data.List (concatMap, intercalate, sort, sortBy, sortOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec (Parsec, parse, sepBy1, sepEndBy1, some)
import Text.Megaparsec.Char (char, letterChar, newline, space, string)

data Food = Food
    { ingredients :: S.Set String
    , allergens :: S.Set String
    }
    deriving (Show)

foodParser :: Parsec Void String Food
foodParser =
    Food
        <$> (S.fromList <$> some letterChar `sepEndBy1` space)
        <* string "(contains "
        <*> (S.fromList <$> some letterChar `sepBy1` string ", ")
        <* char ')'

foodsParser :: Parsec Void String [Food]
foodsParser = sortOn (length . allergens) <$> foodParser `sepEndBy1` newline

computeAllergens :: [Food] -> M.Map (S.Set String) (S.Set String)
computeAllergens = foldl processFood (M.fromList [(S.empty, S.empty)])
  where
    processFood allergensMap (Food ingredients allergens) =
        M.union (M.foldlWithKey expandAllergens allergensMap allergensMap) allergensMap
      where
        expandAllergens current key value = M.insert newKey updatedValue $ M.mapWithKey updateElem current
          where
            newKey = S.union key allergens
            newValue = S.union ingredients value
            updatedValue = maybe newValue (S.intersection newValue) (current M.!? newKey)
            updateElem k v
                | S.isSubsetOf k newKey = S.intersection v updatedValue
                | otherwise = v

ingredientCounts :: [Food] -> M.Map String Int
ingredientCounts foods = foldr (flip (M.insertWith (+)) 1) M.empty $ concatMap (S.toList . ingredients) foods

solvePart1 :: [Food] -> Int
solvePart1 foods = M.foldl (+) 0 $ M.difference counts (M.fromSet id allIngredients)
  where
    allergensMap = computeAllergens foods
    allIngredients = S.unions $ M.elems allergensMap
    counts = ingredientCounts foods

computeIngredientAllergens :: [Food] -> M.Map String String
computeIngredientAllergens foods = M.map (head . S.toList) $ fixedPoint updateMap ingredients
  where
    allergensMap = computeAllergens foods
    allAlergens = S.unions $ M.keys allergensMap
    allIngredients = S.unions $ M.elems allergensMap
    getIngredients allergen = fromMaybe allIngredients (allergensMap M.!? S.singleton allergen)
    ingredients = M.fromSet getIngredients allAlergens
    updateMap m = M.union settled (M.map (`S.difference` settledIngredients) unsettled)
      where
        (settled, unsettled) = M.partition ((== 1) . length) m
        settledIngredients = S.unions $ M.elems settled

solvePart2 :: [Food] -> String
solvePart2 foods = intercalate "," $ map snd $ sortOn fst $ M.assocs allergens
  where
    allergens = computeIngredientAllergens foods

solve :: String -> String
solve content = formatResults part1 part2
  where
    Right foods = parse foodsParser "" content
    part1 = solvePart1 foods
    part2 = solvePart2 foods
