module Day12 (
    solve,
) where

import Utils (
    formatIntResults,
 )

import Control.Monad.State (State, execState, gets, modify)
import Text.Read (readMaybe)

data Direction = North | South | East | West deriving (Show, Eq)

data Instruction = Turn Int | Move (Maybe Direction) Int
    deriving (Eq, Show)

instance Read Instruction where
    readsPrec _ ('N' : xs) = maybe [] (\d -> [(Move (Just North) d, "")]) (readMaybe xs)
    readsPrec _ ('S' : xs) = maybe [] (\d -> [(Move (Just South) d, "")]) (readMaybe xs)
    readsPrec _ ('E' : xs) = maybe [] (\d -> [(Move (Just East) d, "")]) (readMaybe xs)
    readsPrec _ ('W' : xs) = maybe [] (\d -> [(Move (Just West) d, "")]) (readMaybe xs)
    readsPrec _ ('L' : xs) = maybe [] (\a -> [(Turn a, "")]) (readMaybe xs)
    readsPrec _ ('R' : xs) = maybe [] (\a -> [(Turn ((- a) `mod` 360), "")]) (readMaybe xs)
    readsPrec _ ('F' : xs) = maybe [] (\d -> [(Move Nothing d, "")]) (readMaybe xs)
    readsPrec _ _ = []

data Position = Position {lattitude :: Int, longitude :: Int} deriving (Eq, Show)

data ShipState = ShipState
    { position :: Position
    , angle :: Int
    , waypoint :: Position
    }
    deriving (Eq, Show)

type NavigationM = State ShipState

modifyLattitude :: Position -> Int -> Position
modifyLattitude pos delta = pos{lattitude = lattitude pos + delta}

modifyLongitude :: Position -> Int -> Position
modifyLongitude pos delta = pos{longitude = longitude pos + delta}

makePosition :: Int -> Int -> Position
makePosition lat long = Position{lattitude = lat, longitude = long}

initialState :: ShipState
initialState = ShipState{position = makePosition 0 0, waypoint = makePosition 1 10, angle = 0}

getDirection :: ShipState -> Direction
getDirection ShipState{angle = 0} = East
getDirection ShipState{angle = 90} = North
getDirection ShipState{angle = 180} = West
getDirection ShipState{angle = 270} = South
getDirection _ = error "this ship does not go diagonal"

manhattanDistance :: ShipState -> Int
manhattanDistance ShipState{position = Position{lattitude = lat, longitude = lon}} = abs lat + abs lon

computeNewPosition :: Position -> Int -> Direction -> Position
computeNewPosition pos dist North = modifyLattitude pos dist
computeNewPosition pos dist South = modifyLattitude pos (- dist)
computeNewPosition pos dist West = modifyLongitude pos (- dist)
computeNewPosition pos dist East = modifyLongitude pos dist

moveShip :: Int -> Direction -> NavigationM ()
moveShip dist dir = modify (\s -> s{position = computeNewPosition (position s) dist dir})

runInstruction :: Instruction -> NavigationM ()
runInstruction (Move Nothing dist) = gets getDirection >>= moveShip dist
runInstruction (Move (Just dir) dist) = moveShip dist dir
runInstruction (Turn delta) = modify (\s -> s{angle = (angle s + delta) `mod` 360})

turnWaypoint :: Position -> Int -> Position
turnWaypoint Position{lattitude = lat, longitude = long} 90 = Position{lattitude = long, longitude = - lat}
turnWaypoint Position{lattitude = lat, longitude = long} 180 = Position{lattitude = - lat, longitude = - long}
turnWaypoint Position{lattitude = lat, longitude = long} 270 = Position{lattitude = - long, longitude = lat}
turnWaypoint _ angle = error ("waypoint does not only rotate 90, 180 or 270 degrees, not " ++ show angle)

runWaypointInstruction :: Instruction -> NavigationM ()
runWaypointInstruction (Move Nothing dist) = do
    Position{lattitude = waypointLat, longitude = waypointLong} <- gets waypoint
    modify (\s -> s{position = modifyLattitude (position s) (waypointLat * dist)})
    modify (\s -> s{position = modifyLongitude (position s) (waypointLong * dist)})
runWaypointInstruction (Move (Just dir) dist) =
    modify (\s -> s{waypoint = computeNewPosition (waypoint s) dist dir})
runWaypointInstruction (Turn angle) = modify (\s -> s{waypoint = turnWaypoint (waypoint s) angle})

solve :: String -> String
solve content = formatIntResults part1 part2
  where
    instructions = map read $ lines content
    part1 = manhattanDistance $ execState (mapM_ runInstruction instructions) initialState
    part2 = manhattanDistance $ execState (mapM_ runWaypointInstruction instructions) initialState
