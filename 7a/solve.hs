import System.IO
import qualified Data.Map.Strict as Map
import Text.Regex.PCRE
import Text.Regex.Base.RegexLike
import Data.List.Split
import qualified Data.Set as Set
import Data.Set (Set, fromList, size, unions, empty, elems, insert)

main = do
    handle <- openFile "input" ReadMode
    fileContents <- hGetContents handle
    let fileLines = filter (\x -> length x > 0) $ lines fileContents
    let ruleSets = map parseRuleSet fileLines
    let canBeContainedMap = buildCanBeContainedMap ruleSets Map.empty

    let canContain = canContainBags canBeContainedMap (Map.findWithDefault Set.empty "shiny gold" canBeContainedMap)
    let solution = size canContain

    print solution
    hClose handle


type Colour = String
type Rule = (Colour, Int)
type RuleSet = (Colour, [Rule])
type CanBeContainedMap = Map.Map Colour (Set Colour)

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

buildCanBeContainedMap:: [RuleSet] -> CanBeContainedMap -> CanBeContainedMap
buildCanBeContainedMap [] acc = acc
buildCanBeContainedMap ((container, rules):ruleSets) acc = do
    let canContainColours = fromList $ map fst rules
    let newAcc = addToMap container (Set.elems canContainColours) acc
    buildCanBeContainedMap ruleSets newAcc

addToMap:: Colour -> [Colour] -> CanBeContainedMap -> CanBeContainedMap
addToMap _ [] acc = acc
addToMap container (c:canContainColours) canBeContainedMap = do
    let existingContainers = Map.findWithDefault Data.Set.empty c canBeContainedMap
    let updatedContainers = Set.insert container existingContainers
    let newAcc = Map.insert c updatedContainers canBeContainedMap
    addToMap container canContainColours newAcc


canContainBags:: CanBeContainedMap -> Set Colour -> Set Colour
canContainBags canBeContainedMap acc = do
    let newCanContain = Set.union acc $ Set.unions [canContainColour canBeContainedMap colour | colour <- elems acc]
    if size newCanContain == size acc then acc else canContainBags canBeContainedMap newCanContain

canContainColour:: CanBeContainedMap -> Colour -> Set Colour
canContainColour canBeContainedMap colour = Map.findWithDefault Data.Set.empty colour canBeContainedMap