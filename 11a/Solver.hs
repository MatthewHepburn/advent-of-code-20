module Solver where

import Data.Sort
import Data.List
import Data.Array.Unboxed

type Board = UArray (Int, Int) Char
type Position = (Int, Int)
type Bounds = ((Int, Int), (Int, Int))


solve:: String -> Int
solve str = length [ char | char <- elems lastBoard, char == '#']
    where
        lastBoard = getLastBoard startBounds startBoard
        startBoard = parseBoard str
        startBounds = bounds startBoard

parseBoard:: String -> Board
parseBoard s = listArray ((1, 1), (nColumns, nRows)) asSingleLine
    where
        boardLines = filter (\x -> length x > 0) $ lines s
        nRows = length boardLines
        nColumns = maximum $ map length boardLines
        asSingleLine = concat boardLines

showBoard:: Board -> String
showBoard x = unlines chunked
    where
        elements = elems x
        upperBound = snd $ bounds x
        rowLength = fst $ upperBound
        chunked = reverse $ chunk elements rowLength []

chunk:: String -> Int -> [String] -> [String]
chunk [] n acc = acc
chunk str n acc = chunk (drop n str) n ((take n str):acc)

step:: Board -> Bounds -> Board
step board bounds = listArray bounds newValues
    where
        positions = indices board
        newValues = [ newValueForPos board bounds pos | pos <- positions]

getLastBoard:: Bounds -> Board -> Board
getLastBoard bounds lastBoard
    | nextBoard /= lastBoard = getLastBoard bounds nextBoard
    | otherwise = lastBoard
    where
        nextBoard = step lastBoard bounds

allSteps:: Bounds -> Board -> [Board] -> [Board]
allSteps bounds lastBoard acc
    | (elems $! nextBoard) /= (elems $! lastBoard) = allSteps bounds nextBoard newAcc
    | length acc > 2 = reverse acc
    | otherwise = reverse acc
    where
        nextBoard = step lastBoard bounds
        newAcc = nextBoard:acc

inBounds:: Bounds -> Position -> Bool
inBounds bounds (a, b)
    | a < minA || maxA < a = False
    | b < minB || maxB < b = False
    | otherwise = True
    where
        lowerBound = fst bounds
        upperBound = snd bounds
        minA = fst lowerBound
        maxA = fst upperBound
        minB = snd lowerBound
        maxB = snd upperBound

getAtPos:: Board -> Position -> Char
getAtPos board (y, x) = board ! (y, x)

getNeighbourPositions:: Board -> Bounds -> Position -> [Position]
getNeighbourPositions board bounds (y, x)
    -- Bounds check is expensive, so skip it if the top left and bottom left are in bounds (true most of the time)
    | inBounds bounds topLeft && inBounds bounds bottomRight = possibleNeighbours
    | otherwise = filter (\pos -> inBounds bounds pos) possibleNeighbours
    where
        topLeft = (y - 1, x -1)
        bottomRight = (y + 1, x + 1)
        possibleNeighbours = [topLeft, (y - 1, x), (y - 1, x + 1),
                              (y, x - 1), (y, x + 1),
                              (y + 1, x - 1), (y + 1, x), bottomRight]

--
--getNeighbours:: Board -> Position -> [Char]
--getNeighbours b pos = [ getAtPos b pos | pos <- getNeighbourPositions b pos]

-- Try to skip the accesses we don't care about to make this faster
getRelevantNeighbours:: Board  -> Bounds -> Position -> [Char]
getRelevantNeighbours board bounds pos
    | targetValue == '.' = []
    | targetValue == 'L' = getZeroOrOneOccupied board neighbouringPositions
    | otherwise = [ getAtPos board pos | pos <- neighbouringPositions]
    where
        neighbouringPositions = getNeighbourPositions board bounds pos
        targetValue = getAtPos board pos

getZeroOrOneOccupied:: Board -> [Position] -> [Char]
getZeroOrOneOccupied _ [] = []
getZeroOrOneOccupied b (p:pos)
    | getAtPos b p == '#' = ['#']
    | otherwise = getZeroOrOneOccupied b pos

newValueForPos:: Board -> Bounds -> Position -> Char
newValueForPos board bounds pos = newValue curValue neighbours
    where
        curValue = getAtPos board pos
        neighbours = getRelevantNeighbours board bounds pos

newValue:: Char -> [Char] -> Char
newValue char neighbours
    | char == '.' = char
    | char == '#' && occupiedNeighbours >= 4 = 'L'
    | char == 'L' && occupiedNeighbours == 0 = '#'
    | otherwise = char
    where
        occupiedNeighbours = length $ filter (\x -> x == '#') neighbours
