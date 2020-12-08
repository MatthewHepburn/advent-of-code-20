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
    let flipCandidates = [ x | x <- [0..(length instructions) - 1], not $ isNop (instructions !! x)]

    let flippedInstructions = [ (take pos instructions) ++ [(flipInstruction $ instructions !! pos)] ++ (drop (pos + 1) instructions) | pos <- flipCandidates]

    let results = [ executeOrStop (thisInstructions, 0, 0, Map.empty) | thisInstructions <- flippedInstructions ]

    let nonLoops = (filter (\x -> not $ isLoop x) results)

    print $ (map getAccFromResult nonLoops) !! 0

    hClose handle

data Instruction = Nop Int | Jmp Int | Acc Int deriving Show
data Result = Halt Accumulator | Loop | Crash deriving Show

type Accumulator = Int
type ProgramCounter = Int
type LoopFinder = Map.Map Int Bool

type State = ([Instruction], Accumulator, ProgramCounter, LoopFinder)

parseInstruction:: String -> Instruction
parseInstruction x
    | x =~ "acc" = Acc(num)
    | x =~ "jmp" = Jmp(num)
    | otherwise = Nop(num)
    where
        numAsString = (words x) !! 1
        -- read won't parse "+1" even though it can do "-1", so we need to strip out the +
        num = read [char | char <- numAsString, char /= '+'] :: Int

executeOrStop:: State -> Result
executeOrStop (instructions, acc, pc, lf)
    | Map.member pc lf = Loop
    | pc == length instructions = Halt acc
    | pc > length instructions = Crash
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

isNop:: Instruction -> Bool
isNop (Nop a) = True
isNop _ = False

flipInstruction:: Instruction -> Instruction
flipInstruction (Nop x) = Jmp x
flipInstruction (Jmp x) = Nop x
flipInstruction a = a

isLoop:: Result -> Bool
isLoop Loop = True
isLoop _ = False

getAccFromResult:: Result -> Int
getAccFromResult (Halt x) = x
getAccFromResult _ = error "Result was not Halt"