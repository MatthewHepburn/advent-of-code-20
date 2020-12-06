import System.IO
import Data.List.Split
import Data.Set (Set, fromList, size)
main = do
    handle <- openFile "input" ReadMode
    fileContents <- hGetContents handle

    let groupStrings = splitOn "\n\n" fileContents
    let groups = map lines groupStrings
    let answers = map getAnswers groups

    let answerSum = sum (map size answers)

    print (answerSum)
    hClose handle


type Group = [String]
type Answer = Char

getAnswers:: Group -> Set Answer
getAnswers g = do
    let allAnswers = concat g
    fromList allAnswers