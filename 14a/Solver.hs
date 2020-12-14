{-# LANGUAGE TemplateHaskell #-}

module Solver where

import Data.List
import Data.List.Split
import Text.Regex.PCRE
import Text.Regex.Base.RegexLike
import qualified Data.Map.Strict as Map
import Test.QuickCheck


data Instruction = Assign Int Int | Mask String deriving Show
type State = (Map.Map Int Int, String)

solve:: String -> [String]
solve str = map show instructions
    where
        inputLines = filter (\x -> length x > 0) (lines str)
        instructions = map parseInstruction inputLines

parseInstruction:: String -> Instruction
parseInstruction str
    | target =~ "mem" = Assign (read address) (read value)
    | target =~ "mask" = Mask value
    where
        parts = splitOn " = " str
        target = parts !! 0
        value = parts !! 1
        address = target `intersect` "0123456789"

applyMaskToInt:: Int -> String -> Int
applyMaskToInt x mask = fromBinary masked
    where
        xAsBinary = pad $ toBinary x []
        masked = applyMask mask xAsBinary

applyMask:: String -> String -> String -> String
applyMask [] _ acc = reverse acc
applyMask ('X':mask) (y:ys) = applyMask mask ys (y:acc)
applyMask (m:mask) (y:ys) = applyMask mask ys (m:acc)


pad:: String -> String
pad x
    | length x >= 36 = x
    | otherwise = '0':x


prop_toBinaryAndBackAgain:: Int -> Bool
prop_toBinaryAndBackAgain x = abs x == andBack
    where
        binary = toBinary (abs x) []
        andBack = fromBinary binary 0

toBinary:: Int -> String -> String
toBinary 0 [] = "0"
toBinary 0 acc = acc
toBinary 1 acc = toBinary 0 ('1':acc)
toBinary x acc
    | x `mod` 2 == 0 = toBinary divided ('0':acc)
    | x `mod` 2 == 1 = toBinary divided ('1':acc)
    where
        divided = x `div` 2

fromBinary:: String -> Int -> Int
fromBinary [] acc = acc
fromBinary ('0':xs) acc = fromBinary xs acc
fromBinary ('1':xs) acc = fromBinary xs (acc + currentPower)
    where
        currentExponent = (length xs)
        currentPower = 2 ^ currentExponent


return []
runTests = $quickCheckAll