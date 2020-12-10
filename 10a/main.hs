module Main where

import qualified Solver as Solver
import System.IO
import System.Exit

main = do
    testResult <- Solver.runTests
    if not testResult then die "Test failure" else putStrLn ""
    handle <- openFile "input" ReadMode
    fileContents <- hGetContents handle
    let solution = Solver.solve fileContents

    print solution
    hClose handle
