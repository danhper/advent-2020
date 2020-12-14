module Day8 (
    solve,
) where

import Control.Monad.State (State, evalState, gets, modify)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Read (readMaybe)

import Utils (formatIntResults, readSignedInt)

data Instruction = Nop Int | Acc Int | Jmp Int deriving (Show)

data ExecutionState = Running | Success | Failure

instance Read Instruction where
    readsPrec _ rawInstr = maybe [] (\x -> [(x, unwords xs)]) $ parseInstr instrName value
      where
        (instrName : value : xs) = words rawInstr
        parseInstr "nop" _ = Nop <$> readSignedInt value
        parseInstr "jmp" value = Jmp <$> readSignedInt value
        parseInstr "acc" value = Acc <$> readSignedInt value
        parseInstr _ _ = Nothing

newtype Program = Program (M.Map Int Instruction) deriving (Show)

fromList :: [Instruction] -> Program
fromList list = Program $ M.fromList $ zip [0 ..] list

programLength :: Program -> Int
programLength (Program instrs) = M.size instrs

instance Read Program where
    readsPrec _ rawProgram = maybe [] (\x -> [(x, "")]) instructions
      where
        instructions = fromList <$> traverse readMaybe (lines rawProgram)

data ProgramState = ProgramState
    { pc :: Int
    , seenInstructions :: S.Set Int
    , acc :: Int
    }
type ProgramM = State ProgramState

initialState :: ProgramState
initialState = ProgramState{pc = 0, seenInstructions = S.empty, acc = 0}

getInstr :: Program -> ProgramM Instruction
getInstr (Program instrs) = gets $ (M.!) instrs . pc

modifyPC :: Int -> ProgramM ()
modifyPC delta = gets pc >>= \p -> modify (\s -> s{pc = p + delta})

incPC :: ProgramM ()
incPC = modifyPC 1

modifyAcc :: Int -> ProgramM ()
modifyAcc delta = gets acc >>= \a -> modify (\s -> s{acc = a + delta})

addInstruction :: ProgramM ()
addInstruction = modify (\s -> s{seenInstructions = S.insert (pc s) (seenInstructions s)})

runInstruction :: Instruction -> ProgramM ()
runInstruction (Nop _) = incPC
runInstruction (Jmp v) = modifyPC v
runInstruction (Acc v) = incPC >> modifyAcc v

getExecutionState :: Program -> ProgramM ExecutionState
getExecutionState program = gets computeState
  where
    computeState state
        | S.member (pc state) (seenInstructions state) = Failure
        | pc state >= programLength program = Success
        | otherwise = Running

runProgram :: Program -> ProgramM (ExecutionState, Int)
runProgram program = loop
  where
    loop = getExecutionState program >>= runProgram'
    runProgram' Running = addInstruction >> getInstr program >>= runInstruction >> loop
    runProgram' result = gets $ (,) result . acc

findIndices :: Program -> [Int]
findIndices (Program instrs) = M.keys $ M.filter isNopOrJmp instrs
  where
    isNopOrJmp (Nop _) = True
    isNopOrJmp (Jmp _) = True
    isNopOrJmp _ = False

swapNopAndJmp :: Int -> Program -> Program
swapNopAndJmp index (Program instrs) = Program (M.insert index newInstr instrs)
  where
    newInstr = replace $ instrs M.! index
    replace (Nop v) = Jmp v
    replace (Jmp v) = Nop v
    replace instr = instr

solvePart2 :: Program -> Int
solvePart2 program = findProgram candidateIndices
  where
    candidateIndices = findIndices program
    findProgram [] = 0
    findProgram (x : xs) = case runModified x of
        (Success, res) -> res
        (Failure, _) -> findProgram xs
    runModified index = evalState (runProgram (swapNopAndJmp index program)) initialState

solve :: String -> String
solve content = formatIntResults part1 part2
  where
    program = read content
    (_, part1) = evalState (runProgram program) initialState
    part2 = solvePart2 program
