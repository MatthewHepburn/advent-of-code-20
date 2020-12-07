import System.IO
import qualified Data.Map.Strict as Map
import Text.Regex.PCRE
import Text.Regex.Base.RegexLike
import Data.List.Split
import qualified Data.Set as Set

main = do
    handle <- openFile "input" ReadMode
    fileContents <- hGetContents handle
    let fileLines = filter (\x -> length x > 0) $ lines fileContents
    let ruleSets = Map.fromList $ map parseRuleSet fileLines

    let contentsCount = getContentsCount ruleSets ["shiny gold"] 0

    print contentsCount
    hClose handle


type Bag = String
type Colour = String
type Rule = (Colour, Int)
type RuleSet = (Colour, [Rule])
type RuleSets = Map.Map Colour [Rule]

-- posh indigo bags contain 1 clear plum bag, 2 dotted red bags, 1 dull crimson bag, 4 dotted bronze bags.
parseRuleSet:: String -> RuleSet
parseRuleSet r = do
    let ruleWords = words r
    let subjectColour = (ruleWords !! 0) ++ " " ++ (ruleWords !! 1)
    let ruleString = getAllTextSubmatches (r =~ "contains? (.*)") !! 1
    (subjectColour, parseRules ruleString)


parseRules:: String -> [Rule]
parseRules x
    | x =~ "no other bags" = []
    | otherwise = do
        let parts = extractParts x
        map parseRule parts
    where
        extractParts x = if x =~ "," then splitOn "," x else [x]
        parseRule x = do
            let ruleWords = words x
            let colour =  (ruleWords !! 1) ++ " " ++ (ruleWords !! 2)
            (colour, read $ ruleWords !! 0)

getContentsCount:: RuleSets -> [Bag] -> Int -> Int
getContentsCount _ [] acc = acc
getContentsCount ruleSets (b:bags) acc = do
    let newBagRules = Map.findWithDefault [] b ruleSets
    let newBags = concat [take n $ repeat newBagColour | (newBagColour, n) <- newBagRules]
    let allBags = newBags ++ bags
    let newAcc = acc + length newBags
    getContentsCount ruleSets allBags newAcc