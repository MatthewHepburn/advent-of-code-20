import System.IO

main = do
    handle <- openFile "input" ReadMode
    rawContents <- hGetContents handle
    let problemInput = inputToList rawContents
   
    let solution = solve problemInput
    print solution
    hClose handle

inputToList:: String -> [Integer]
inputToList x = [read y :: Integer | y <- lines x] 

solve:: [Integer] -> Integer
solve x = do
    let combinations = getCombinations x []
    let validCombinations = [(a,b) | (a,b) <- combinations, a + b == 2020]
    let firstSolution = head validCombinations
    (fst firstSolution) * (snd firstSolution)


getCombinations:: [Integer] -> [(Integer, Integer)] -> [(Integer, Integer)]
getCombinations [] ys = ys
getCombinations (a:[]) ys = ys
getCombinations (a:xs) ys = getCombinations xs ([(a, x) | x <- xs ] ++ ys)
