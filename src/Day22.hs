module Day22 (
    solve,
) where

import Utils (formatResults)

import qualified Data.Set as S
import Data.Void (Void)
import qualified Deque.Strict as Q
import GHC.Exts (IsList (toList))
import Text.Megaparsec (Parsec, anySingle, parse, sepEndBy1, skipManyTill, some)
import Text.Megaparsec.Char (digitChar, newline, string)

type Hand = Q.Deque Int
data GameState = GameState
    { player :: Hand
    , opponent :: Hand
    , ended :: Bool
    , seen :: S.Set ([Int], [Int])
    }
    deriving (Show)

type Turn = GameState -> (GameState, Bool)
type Hands = ((Int, Hand), (Int, Hand))

gameStateParser :: Parsec Void String GameState
gameStateParser = GameState <$> parsedHand <* newline <*> parsedHand <*> return False <*> return S.empty
  where
    hand = skipManyTill anySingle newline *> (read <$> some digitChar) `sepEndBy1` newline
    parsedHand = (`Q.fromConsAndSnocLists` []) <$> hand

getTurnCards :: GameState -> ((Int, Hand), (Int, Hand))
getTurnCards GameState{player = player, opponent = opponent} = (playerTurn, opponentTurn)
  where
    Just playerTurn = Q.uncons player
    Just opponentTurn = Q.uncons opponent

updateHands :: GameState -> Bool -> Hands -> GameState
updateHands state True ((playerCard, nextPlayerHand), (opponentCard, nextOpponentHand)) =
    state{player = Q.snoc opponentCard $ Q.snoc playerCard nextPlayerHand, opponent = nextOpponentHand}
updateHands state False ((playerCard, nextPlayerHand), (opponentCard, nextOpponentHand)) =
    state{player = nextPlayerHand, opponent = Q.snoc playerCard $ Q.snoc opponentCard nextOpponentHand}

simpleTurn :: GameState -> (GameState, Bool)
simpleTurn state = (updateHands state p1Wins hands, p1Wins)
  where
    hands@((playerCard, nextPlayerHand), (opponentCard, nextOpponentHand)) = getTurnCards state
    p1Wins = playerCard > opponentCard

complexTurn :: GameState -> (GameState, Bool)
complexTurn state@GameState{player = player, opponent = opponent, seen = seen} =
    complexTurn' (S.member configuration seen) isRecursive
  where
    configuration = (toList player, toList opponent)
    hands@((playerCard, nextPlayerHand), (opponentCard, nextOpponentHand)) = getTurnCards state
    isRecursive = length nextPlayerHand >= playerCard && length nextOpponentHand >= opponentCard
    newState = state{seen = S.insert configuration seen}
    complexTurn' True _ = (updateHands newState{ended = True} True hands, True)
    complexTurn' _ False = simpleTurn newState
    complexTurn' _ True = (updateHands newState p1Wins hands, p1Wins)
      where
        childState = GameState (Q.take playerCard nextPlayerHand) (Q.take opponentCard nextOpponentHand) False S.empty
        (p1Wins, _) = runGame complexTurn childState

shouldStop :: GameState -> Bool
shouldStop state = ended state || Q.null (player state) || Q.null (opponent state)

runGame :: Turn -> GameState -> (Bool, Hand)
runGame turn state = if shouldStop state then getWinner state else runGame turn (fst $ turn state)
  where
    getWinner GameState{player = player, opponent = opponent} =
        if Q.null player then (False, opponent) else (True, player)

computeResult :: Hand -> Int
computeResult hand = snd $ foldl computeScore (length hand, 0) hand
  where
    computeScore (idx, total) value = (idx - 1, total + value * idx)

solve :: String -> String
solve content = formatResults part1 part2
  where
    Right initialState = parse gameStateParser "" content
    getResult turn = computeResult $ snd $ runGame turn initialState
    part1 = getResult simpleTurn
    part2 = getResult complexTurn
