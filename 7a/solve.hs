import System.IO
import qualified Data.Map.Strict as Map
import Text.Regex.PCRE
import Text.Regex.Base.RegexLike

main = do
    handle <- openFile "testinput" ReadMode
    fileContents <- hGetContents handle
    let fileLines = filter (\x -> length x > 0) $ lines fileContents
    let ruleSets = map parseRuleSet fileLines

    print (ruleSets)
    hClose handle


type Colour = String
type Rule = (Colour, Int)
type RuleSet = (Colour, String)

-- posh indigo bags contain 1 clear plum bag, 2 dotted red bags, 1 dull crimson bag, 4 dotted bronze bags.
parseRuleSet:: String -> RuleSet
parseRuleSet r = do
    let ruleWords = words r
    let subjectColour = (ruleWords !! 0) ++ " " ++ (ruleWords !! 1)
    let ruleString = getAllTextSubmatches (r =~ "contains? (.*)") !! 1
    (subjectColour, parseRules ruleString)


parseRules:: String -> String
parseRules x = x