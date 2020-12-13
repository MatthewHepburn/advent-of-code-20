module Main where

import qualified Solver as Solver
import System.IO
import System.Exit

findLast:: [Solver.Board] -> Solver.Board
findLast (a:[]) = a
findLast (a:b:cs)
    | a == b = a
    | otherwise = findLast (b:cs)

main = do
    handle <- openFile "input" ReadMode
    fileContents <- hGetContents handle

    print $ Solver.solve fileContents
    hClose handle


