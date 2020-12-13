module Main where

import qualified Solver as Solver
import System.IO
import System.Exit

main = do
    handle <- openFile "input" ReadMode
    fileContents <- hGetContents handle

    print $ Solver.solve fileContents
    hClose handle


