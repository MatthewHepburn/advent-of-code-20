module Main where

import qualified Solver as Solver
import System.IO
import System.Exit

main = do
    testResult <- Solver.runTests
    if not testResult then die "Test failure" else return()
    handle <- openFile "input" ReadMode
    fileContents <- hGetContents handle

    print $ Solver.solve fileContents
    hClose handle


