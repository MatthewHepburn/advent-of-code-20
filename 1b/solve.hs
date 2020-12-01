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
    let c2 = getCombinations x []
    let c3 = getCombinationsWithThree c2 x []
    let validCombinations = [(a,b,c) | (a,b,c) <- c3, a + b + c == 2020]
    tupleProduct(head validCombinations)


getCombinations:: [Integer] -> [(Integer, Integer)] -> [(Integer, Integer)]
getCombinations [] ys = ys
getCombinations (a:[]) ys = ys
getCombinations (a:xs) ys = getCombinations xs ([(a, x) | x <- xs ] ++ ys)

getCombinationsWithThree:: [(Integer, Integer)] -> [Integer] -> [(Integer, Integer, Integer)] -> [(Integer, Integer, Integer)]
getCombinationsWithThree [] _ ys = ys
getCombinationsWithThree _ [] ys = ys
getCombinationsWithThree pairs (c:cs) ys = getCombinationsWithThree pairs cs ([(a, b, c) | (a, b) <- pairs] ++ ys)

tupleProduct:: (Integer, Integer, Integer) -> Integer
tupleProduct (a,b,c) = a * b * c
