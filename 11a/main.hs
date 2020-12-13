module Main where

import qualified Solver as Solver
import System.IO
import System.Exit
import Data.Array.Unboxed

findLast:: [Solver.Board] -> Solver.Board
findLast (a:[]) = a
findLast (a:b:cs)
    | a == b = a
    | otherwise = findLast (b:cs)

main = do
    handle <- openFile "input" ReadMode
    fileContents <- hGetContents handle

    let startBoard = Solver.parseBoard fileContents
    let thisBounds = bounds startBoard
    let iterations = [0..]
    let allBoards = scanl  (\b i -> Solver.step b thisBounds) startBoard iterations

    putStr $ Solver.showBoard (findLast allBoards)

    print $ Solver.countOccupied (findLast allBoards)
    hClose handle


