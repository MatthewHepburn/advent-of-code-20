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
solve str = serviceToCatch * (timeToCatchIt - startTime)
    where
        inputLines = filter (\x -> length x > 0) (lines str)
        startTime = read (inputLines !! 0) :: Int
        services = splitOn "," (inputLines !! 1)
        validServices = [read x :: Int | x <- services, x /= "x"]
        firstPossibleByService = [(bus, getFirstPossible startTime bus) | bus <- validServices]
        (serviceToCatch, timeToCatchIt) = foldl (\(busA, timeA) (busB, timeB) -> if timeA > timeB then (busB, timeB) else (busA, timeA)) (head firstPossibleByService) (tail firstPossibleByService)

getFirstPossible:: Timestamp -> Bus -> Timestamp
-- From an infinite list of bus times, find the first which is possible
getFirstPossible startTime bus = foldr (\a b -> if isPossible a then a else b) 0 allBusTimes
    where
        allBusTimes = [0,bus..]
        isPossible x = x >= startTime

return []
runTests = $quickCheckAll