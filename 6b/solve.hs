import System.IO
import Data.List.Split
import Data.Set (Set, fromList, size, intersection)
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
getAnswers [] = fromList []
getAnswers (x:[]) = fromList x
getAnswers (x:xs) = getAnswersRec xs (fromList x)

getAnswersRec:: Group -> Set Answer -> Set Answer
getAnswersRec [] acc = acc
getAnswersRec (x:xs) acc = do
    let thisSet = fromList x
    let thisIntersection = intersection acc thisSet
    getAnswersRec xs thisIntersection