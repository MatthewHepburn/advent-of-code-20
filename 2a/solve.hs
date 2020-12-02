import System.IO
import Data.List.Split

main = do
    handle <- openFile "input" ReadMode
    fileContents <- hGetContents handle
    let problemInput = lines fileContents
    let passwordList = map parseLine problemInput
    let solution = length (filter isValid passwordList)
   
    print solution
    hClose handle

type Password = String
type Range = (Integer, Integer)
type Policy = (Range, Char)


parseLine:: String -> (Policy, Password)
parseLine x = do
    let parts = words x
    (((parseRange (parts!!0)), (parseChar (parts!!1))), parts!!2)

parseRange:: String -> Range
parseRange x = do
    let parts = splitOn "-" x
    ((read (parts !! 0) :: Integer), (read (parts !! 1) :: Integer))

parseChar:: String -> Char
parseChar x = head x

isValid:: (Policy, Password) -> Bool
isValid (policy, password) = do
    let range = fst policy
        char = snd policy
        count = countOccurances char password 0
    (fst range) <= count && count <= (snd range)
    

countOccurances:: Char -> String -> Integer -> Integer
countOccurances _ [] acc = acc
countOccurances c (x:xs) acc
    | c == x = countOccurances c xs (acc +1)
    | otherwise = countOccurances c xs acc
 
