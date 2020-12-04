import System.IO
import Data.List.Split

main = do
    handle <- openFile "input" ReadMode
    fileContents <- hGetContents handle
    
    let passports = marshallInput fileContents
    let validPassports = filter isValid passports
    
    print (length validPassports)
    hClose handle

type Field = (String, String)
type Passport = [Field]

marshallInput :: String -> [Passport]
marshallInput input = do
    let passportsAsStrings = splitOn "\n\n" input
    let passportsAsLists =  map (\y -> splitOneOf "\n " y) passportsAsStrings
    map (\p -> [ parseField str | str <- p, length str > 0]) passportsAsLists 

parseField :: String -> Field
parseField str = do
    let parts = splitOn ":" str
    (parts !! 0, parts !! 1)

requiredFields:: [String]
requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isValid:: Passport -> Bool
isValid [] = False
isValid passport = hasFields passport requiredFields

hasFields:: Passport -> [String] -> Bool
hasFields _ [] = True
hasFields passport (f:fs)
    | lookup f passport == Nothing = False
    | otherwise = hasFields passport fs
