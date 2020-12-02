import System.IO
import Data.List.Split

main = do
    handle <- openFile "testinput" ReadMode
    fileContents <- hGetContents handle
    let problemInput = lines fileContents
    let passwordList = map parseLine problemInput
    print passwordList
    let invalid = filter (\x -> not $ isValid x) passwordList
    let solution = length (filter isValid passwordList)
   
    print invalid
    hClose handle

type Password = String
type Positions = (Int, Int)
type Policy = (Positions, Char)


parseLine:: String -> (Policy, Password)
parseLine x = do
    let parts = words x
    (((parsePositions (parts!!0)), (parseChar (parts!!1))), parts!!2)

parsePositions:: String -> Positions
parsePositions x = do
    let parts = splitOn "-" x
    ((read (parts !! 0) :: Int), (read (parts !! 1) :: Int))

parseChar:: String -> Char
parseChar x = head x

isValid:: (Policy, Password) -> Bool
isValid (policy, password) = do
    let positions = fst policy
        char = snd policy
    (hasCharAtPos char (fst positions) password) `xor` (hasCharAtPos char (snd positions) password)

hasCharAtPos:: Char -> Int -> Password -> Bool
hasCharAtPos char pos password 
    | length password > pos = False
    | otherwise = password !! (pos - 1) == char

// TODO: QUICK CHECK HERE
xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b) 
