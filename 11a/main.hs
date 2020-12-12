module Main where

import qualified Solver as Solver
import System.IO
import System.Exit
import Data.Array

main = do
    handle <- openFile "input" ReadMode
    fileContents <- hGetContents handle

--    let startBoard = Solver.parseBoard fileContents
--    let thisBounds = bounds startBoard
----    let allBoards = Solver.allSteps (bounds startBoard) startBoard [startBoard]
--
--    putStrLn "Start = "
--    putStr $ Solver.showBoard startBoard
--    putStrLn "--------"
--
--    let board1 = Solver.step startBoard thisBounds
--    putStrLn "After 1 = "
--    putStr $ Solver.showBoard board1
--    putStrLn "--------"
--
--    let board2 = Solver.step board1 thisBounds
--    putStrLn "After 2 = "
--    putStr $ Solver.showBoard board2
--    putStrLn "--------"
--
--    let board3 = Solver.step board2 thisBounds
--    putStrLn "After 3 = "
--    putStr $ Solver.showBoard board3
--    putStrLn "--------"
--
--
--    let board4 = Solver.step board3 thisBounds
--    putStrLn "After 2 = "
--    putStr $ Solver.showBoard board4
--    putStrLn "--------"

--    putStrLn "End = "
--    putStr $ Solver.showBoard $ last allBoards
--    putStrLn "--------"


    let solution = Solver.solve fileContents
--
    print solution
    hClose handle


