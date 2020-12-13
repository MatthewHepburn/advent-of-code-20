{-# LANGUAGE TemplateHaskell #-}

module Solver where

import Data.List
import Text.Regex.PCRE
import Text.Regex.Base.RegexLike
import Test.QuickCheck

type Position = (Int, Int)
type Waypoint = (Int, Int)
type Direction = (Int, Int)
type State = (Position, Waypoint)

data Instruction = Fwd Int | Mov Direction Int | Trn Int deriving Show

solve:: String -> [String]
solve str = (("Start = " ++ show startState):annotatedStates)++ ([show $ manhattanDistance finalPos])
    where
        instructionLines = filter (\x -> length x > 0) (lines str)
        instructions = map parseInstruction instructionLines
        startState = ((0,0), (10, 1))
        states = scanl (\state instruction -> step instruction state) startState instructions
        finalPos = fst $ last states
        annotatedStates = [show instruction ++ " ->  \tS@ \t" ++ (pprintTuple pos) ++ "\tW@ \t" ++ (pprintTuple waypoint) | ((pos, waypoint), instruction) <- zip (tail states) instructions]

move:: Direction -> Position -> Int -> Position
move (dirX, dirY) (posX, posY) dist = (posX + distX, posY + distY)
    where
        (distX, distY) = (dirX * dist, dirY * dist)

moveToWaypoint:: Int -> Position -> Waypoint -> Position
moveToWaypoint 0 pos _ = pos
moveToWaypoint n (p1, p2) (w1, w2) = moveToWaypoint (n-1) (p1 + w1, p2 + w2) (w1, w2)

-- Check that 4 rotations return the original result
prop_4rotationsAreIdentity:: Waypoint -> Bool
prop_4rotationsAreIdentity waypoint = rotateRightNTimes 4 waypoint == waypoint

rotateRightNTimes:: Int -> Waypoint -> Waypoint
rotateRightNTimes 0 waypoint = waypoint
rotateRightNTimes n waypoint = rotateRightNTimes (n - 1) (rotateRight waypoint)

rotateRight:: Waypoint -> Waypoint
rotateRight (x, y) = (y, -x)

step:: Instruction -> State -> State
step (Fwd times) (position, waypoint) = ((moveToWaypoint times position waypoint), waypoint)
step (Mov movDirection distance) (position, waypoint) = (position, (move movDirection waypoint distance))
step (Trn rightTurns) (position, waypoint) = (position, rotateRightNTimes rightTurns waypoint)

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

pprintTuple:: (Int, Int) -> String
pprintTuple (a, b) = (printEw a) ++ " \t" ++ (printNs b)
    where
        printEw x
            | x >= 0 = "E: " ++ (show x)
            | otherwise = "W: " ++ (show (-x))
        printNs x
            | x >= 0 = "N: " ++ (show x)
            | otherwise = "S: " ++ (show (-x))

return []
runTests = $quickCheckAll