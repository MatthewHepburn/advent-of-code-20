import System.IO
import qualified Data.Map.Strict as Map
import Text.Regex.PCRE
import Text.Regex.Base.RegexLike
import Data.List.Split
import qualified Data.Set as Set

main = do
    handle <- openFile "input" ReadMode
    fileContents <- hGetContents handle
    let fileLines = filter (\x -> length x > 0) $ lines fileContents
    let instructions = map parseInstruction fileLines

    let solution = executeOrStop (instructions, 0, 0, Map.empty)

    print solution
    hClose handle

data Instruction = Nop | Jmp Int | Acc Int deriving Show

type Accumulator = Int
type ProgramCounter = Int
type LoopFinder = Map.Map Int Bool

type State = ([Instruction], Accumulator, ProgramCounter, LoopFinder)

parseInstruction:: String -> Instruction
parseInstruction x
    | x =~ "acc" = Acc(num)
    | x =~ "jmp" = Jmp(num)
    | otherwise = Nop
    where
        numAsString = (words x) !! 1
        -- read won't parse "+1" even though it can do "-1", so we need to strip out the +
        num = read [char | char <- numAsString, char /= '+'] :: Int

executeOrStop:: State -> Accumulator
executeOrStop (instructions, acc, pc, lf)
    | Map.member pc lf = acc -- we've been to this pc value before, so return the accumulator
    | otherwise = do
        let newState = execute (instructions, acc, pc, lf)
        executeOrStop newState

execute:: State -> State
execute (instructions, acc, pc, lf) = (instructions, newAcc, newPc, newLf)
    where
        inst = instructions !! pc
        newLf = Map.insert pc True lf
        newAcc = acc + getAccChange inst
        newPc = pc + getPcChange inst

getAccChange:: Instruction -> Int
getAccChange (Acc x) = x
getAccChange _ = 0

getPcChange:: Instruction -> Int
getPcChange (Jmp x) = x
getPcChange _ = 1 -- by default advance to the next instruction

getAcc:: State -> Accumulator
getAcc (_, acc, _, _) = acc

getPc:: State -> ProgramCounter
getPc (_, _, pc, _) = pc