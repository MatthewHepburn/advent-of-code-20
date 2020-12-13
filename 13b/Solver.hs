{-# LANGUAGE TemplateHaskell #-}

module Solver where

import Data.List
import Data.List.Split
import Text.Regex.PCRE
import Text.Regex.Base.RegexLike
import Test.QuickCheck

type Timestamp = Int
type Bus = Int

solve:: String -> Int
solve str = getFirstToFit services
    where
        inputLines = filter (\x -> length x > 0) (lines str)
        startTime = read (inputLines !! 0) :: Int
        serviceStrings = splitOn "," (inputLines !! 1)
        services = [ if (x /= "x") then  read x :: Int else 1 | x <- serviceStrings]

-- Test the given examples give the expected answers
prop_examples:: Bool
prop_examples = and examples
    where
        examples = [
            (getFirstToFit [17,1,13,19] == 3417),
            (getFirstToFit [67,7,59,61] == 754018),
            (getFirstToFit [67,1,7,59,61] == 779210)]

getFirstToFit:: [Bus] -> Timestamp
getFirstToFit buses = foldr (\a b -> if fitsSequence a buses then a else b) (head allTimes) (tail allTimes)
    where
        increment = buses !! 0
        allTimes = [increment,increment * 2..]

fitsSequence:: Timestamp -> [Bus] -> Bool
fitsSequence _ [] = True
fitsSequence time (b:buses)
    | time `mod` b == 0 = fitsSequence (time + 1) buses
    | otherwise = False

return []
runTests = $quickCheckAll