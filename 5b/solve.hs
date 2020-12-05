import System.IO
import Data.Sort
import Data.Maybe

main = do
    handle <- openFile "input" ReadMode
    fileContents <- hGetContents handle

    let rowCount = 128
    let colCount = 8

    let boardingCards = [(take 7 line, drop 7 line) | line <- lines fileContents, length line > 0]
    let positions = map getPosition boardingCards
    let ids = sort $ map getId positions
    let missing = findMissing ids

    print (fromJust missing)
    hClose handle


type RowNumber = Int
type ColNumber = Int
type Seat = (RowNumber, ColNumber)
type BoardingCard = (String, String)

getNumber:: (Int, Int) -> String -> Int
getNumber (lower,upper) [] = lower
getNumber (lower, upper) (x:xs)
    | x == 'F' || x == 'L' = getNumber (lower, lower + increment) xs
    | otherwise = getNumber (lower + increment, upper) xs
    where
        increment = (upper - lower) `div` 2

getPosition:: BoardingCard -> Seat
getPosition (rowBit, colBit) = do
    let rowNumber = getNumber (0, 128) rowBit
    let colNumber = getNumber (0, 8) colBit
    (rowNumber, colNumber)

getId:: Seat -> Int
getId (row, col) = (8 * row) + col

findMissing:: [Int] -> Maybe Int
findMissing (a:b:cs)
    | a + 1 == b = findMissing (b:cs)
    | otherwise = Just (a + 1)
findMissing _ = Nothing