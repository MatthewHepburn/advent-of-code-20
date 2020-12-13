module Solver where

import Data.List
import Text.Regex.PCRE
import Text.Regex.Base.RegexLike

type Position = (Int, Int)
type Direction = (Int, Int)
type State = (Position, Direction)

data Instruction = Fwd Int | Mov Direction Int | Trn Int deriving Show

solve:: String -> Int
solve str = manhattanDistance $ fst $ last states
    where
        instructionLines = filter (\x -> length x > 0) (lines str)
        instructions = map parseInstruction instructionLines
        startState = ((0,0), (1, 0))
        states = scanl (\state instruction -> step instruction state) startState instructions

move:: Direction -> Position -> Int -> Position
move (dirX, dirY) (posX, posY) dist = (posX + distX, posY + distY)
    where
        (distX, distY) = (dirX * dist, dirY * dist)

turnRightNTimes:: Int -> Direction -> Direction
turnRightNTimes 0 direction = direction
turnRightNTimes n direction = turnRightNTimes (n - 1) (turnRight direction)

turnRight:: Direction -> Direction
turnRight (0, 1) = (1, 0)
turnRight (1, 0) = (0, -1)
turnRight (0, -1) = (-1, 0)
turnRight (-1, 0) = (0, 1)

step:: Instruction -> State -> State
step (Fwd distance) (position, direction) = ((move direction position distance), direction)
step (Mov movDirection distance) (position, direction) = ((move movDirection position distance), direction)
step (Trn rightTurns) (position, direction) = (position, turnRightNTimes rightTurns direction)

parseInstruction:: String -> Instruction
parseInstruction str
    | symbol == "F" = Fwd number
    | symbol == "N" = Mov (0, 1) number
    | symbol == "E" = Mov (1, 0) number
    | symbol == "S" = Mov (0, -1) number
    | symbol == "W" = Mov (-1, 0) number
    | symbol == "R" = Trn (number `div` 90)
    | symbol == "L" = Trn ((-1 * number `div` 90) `mod` 4)
    where
        symbol = take 1 str
        number = read (drop 1 str) :: Int

manhattanDistance:: Position -> Int
manhattanDistance (a, b) = (abs a) + (abs b)