import System.IO
import qualified Data.Map.Strict as Map
import Text.Regex.PCRE
import Text.Regex.Base.RegexLike
import Data.List.Split
import qualified Data.Set as Set
import Data.Maybe

main = do
    handle <- openFile "input" ReadMode
    fileContents <- hGetContents handle
    let fileLines = filter (\x -> length x > 0) $ lines fileContents
    let numbers = map (\x -> read x :: Int) fileLines

    let preambleSize = 25
    let preamble = take preambleSize numbers
    let payload = drop preambleSize numbers

    let maybeSol = solve preamble payload

    print $ fromJust maybeSol

    hClose handle

solve:: [Int] -> [Int] -> Maybe Int
solve _ [] = Nothing
solve buffer (x:xs)
    | hasPair buffer x = solve newBuffer xs
    | otherwise = Just x
    where
        newBuffer = (drop 1 buffer) ++ [x]

hasPair:: [Int] -> Int -> Bool
hasPair buffer target = length [(a,b) | a <- buffer, b <- buffer, a /= b && a + b == target] > 0