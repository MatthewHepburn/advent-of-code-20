 {-# LANGUAGE TemplateHaskell #-}

module Solver where

import qualified Data.Map.Strict as Map
import Data.Sort
import Data.List.Split
import qualified Data.Set as Set
import Data.Maybe
import Test.QuickCheck

solve:: String -> Int
solve fileContents = do
    let fileLines = filter (\x -> length x > 0) $ lines fileContents
    let adaptors = sort $ map (\x -> read x :: Int) fileLines

    let joltageDiffs = getJoltageDiffs adaptors 0 []
    let oneJumps = length $ filter (\x -> x == 1) joltageDiffs
    let threeJumps = 1 + (length $ filter (\x -> x == 3) joltageDiffs) -- Count the bonus adaptor

    oneJumps * threeJumps

prop_JoltageDiffsSum:: [Int] -> Bool
prop_JoltageDiffsSum xs = sum joltageDiffs == expected
    where
        positiveXs = map abs xs
        joltageDiffs = getJoltageDiffs (sort positiveXs) 0 []
        expected = if xs == [] then 0 else maximum positiveXs

getJoltageDiffs:: [Int] -> Int -> [Int] -> [Int]
getJoltageDiffs [] _ acc = acc
getJoltageDiffs (x:xs) lastJoltage acc = getJoltageDiffs xs x ((x - lastJoltage):acc)

return []
runTests = $quickCheckAll