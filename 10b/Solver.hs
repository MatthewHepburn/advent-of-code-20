 {-# LANGUAGE TemplateHaskell #-}

module Solver where

import Data.Sort
import Test.QuickCheck
import Data.List

solve:: String -> Int
solve fileContents = do
    let fileLines = filter (\x -> length x > 0) $ lines fileContents
    let adaptors = sort $ map (\x -> read x :: Int) fileLines
    let adaptorsAndDevice = adaptors ++ [((last adaptors) + 3)]

    let joltageDiffs = reverse $ getJoltageDiffs adaptorsAndDevice 0 []
    let joltageDiffsIndexed = zip joltageDiffs [0..]

    -- 3 Jump points are special: all solutions must utilize the adaptors on either side of the jump
    -- So find the indices of these jumps
    let threeJumpIndices = [ i + 1 | (jump, i) <- joltageDiffsIndexed, jump == 3]

    -- Chunk the list on these indices to give us something manageable to work with
    let chunks = chunkOnIndices (0:adaptorsAndDevice) threeJumpIndices 0 []

    -- Calculate the number of valid paths through each chunk
    let passesThroughChunks = map getPassesThroughCount chunks

    -- Multiply the number of paths through each chunk to get the total number of possible paths
    product passesThroughChunks


-- Test that the diffs of a positive sorted list sum to the maximum value of the list
prop_JoltageDiffsSum:: [Int] -> Bool
prop_JoltageDiffsSum xs = sum joltageDiffs == expected
    where
        positiveXs = map abs xs
        joltageDiffs = getJoltageDiffs (sort positiveXs) 0 []
        expected = if xs == [] then 0 else maximum positiveXs

getJoltageDiffs:: [Int] -> Int -> [Int] -> [Int]
getJoltageDiffs [] _ acc = acc
getJoltageDiffs (x:xs) lastJoltage acc = getJoltageDiffs xs x ((x - lastJoltage):acc)

chunkOnIndices:: [Int] -> [Int] -> Int -> [[Int]] -> [[Int]]
chunkOnIndices [] _ _ acc = acc
chunkOnIndices xs []  last acc = acc ++ [drop last xs]
chunkOnIndices xs (i:is) last acc = chunkOnIndices xs is i(acc ++ [chunk])
    where
        chunk = take (i - last) (drop (last) xs)

getPassesThroughCount:: [Int] -> Int
getPassesThroughCount xs
    | length xs < 3 = 1
    | otherwise = length $ filter isValid allPaths
        where
            skippable = (length xs) - 2 -- can't skip the ends
            skippablePerms = subsequences [1..((length xs) - 2) ]
            allPaths = map (\is -> applySkips is xs) skippablePerms

applySkips:: [Int] -> [Int] -> [Int]
applySkips toSkip xs = [ x | (i, x) <- zip [0..] xs, not $ i `elem` toSkip]

isValid:: [Int] -> Bool
isValid [] = False
isValid (x:xs) = length violating == 0
    where
        violating = filter (\x -> x > 3) $ getJoltageDiffs xs x []

return []
runTests = $quickCheckAll