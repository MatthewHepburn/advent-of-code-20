module Solver where

import Data.Sort
import Data.List
import Data.Array.Unboxed

type Board = UArray (Int, Int) Char
type Position = (Int, Int)
type Bounds = ((Int, Int), (Int, Int))


solve:: String -> Int
solve str = countOccupied lastBoard
    where
        lastBoard = findLast allBoards
        allBoards = scanl  (\b i -> step b startBounds) startBoard [0..]
        startBoard = parseBoard str
        startBounds = bounds startBoard

parseBoard:: String -> Board
parseBoard s = listArray ((1, 1), (nRows, nColumns)) asSingleLine
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
        rowLength = snd $ upperBound
        chunked = reverse $ chunk elements rowLength []

chunk:: String -> Int -> [String] -> [String]
chunk [] n acc = acc
chunk str n acc = chunk (drop n str) n ((take n str):acc)

step:: Board -> Bounds -> Board
step board bounds = listArray bounds newValues
    where
        positions = indices board
        newValues = [ newValueForPos board bounds pos | pos <- positions]

findLast:: [Board] -> Board
findLast (a:[]) = a
findLast (a:b:cs)
    | a == b = a
    | otherwise = findLast (b:cs)

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
getAtPos board (a, b) = board ! (a, b)

getNeighbourPositions:: Board -> Bounds -> Position -> [Position]
getNeighbourPositions board bounds (y, x)
    -- Bounds check is expensive, so skip it if the top left and bottom left are in bounds (true most of the time)
    | inBounds bounds topLeft && inBounds bounds bottomRight = possibleNeighbours
    | otherwise = filter (\pos -> inBounds bounds pos) possibleNeighbours
    where
        topLeft = (y - 1, x -1)
        bottomRight = (y + 1, x + 1)
        possibleNeighbours = [(y - 1, x -1), (y - 1, x), (y - 1, x + 1),
                              (y, x - 1), (y, x + 1),
                              (y + 1, x - 1), (y + 1, x), (y + 1, x + 1)]


getNeighbours:: Board -> Bounds -> Position -> [Char]
getNeighbours board bounds pos = [ getAtPos board pos | pos <- getNeighbourPositions board bounds pos]

newValueForPos:: Board -> Bounds -> Position -> Char
newValueForPos board bounds pos = newValue curValue neighbours
    where
        curValue = getAtPos board pos
        neighbours = getNeighbours board bounds pos

newValue:: Char -> [Char] -> Char
newValue char neighbours
    | char == '.' = char
    | char == '#' && occupiedNeighbours >= 4 = 'L'
    | char == 'L' && occupiedNeighbours == 0 = '#'
    | otherwise = char
    where
        occupiedNeighbours = length $ filter (\x -> x == '#') neighbours

countOccupied:: Board -> Int
countOccupied b = length [ char | char <- elems b, char == '#']