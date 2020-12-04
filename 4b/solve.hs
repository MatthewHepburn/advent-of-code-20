import System.IO
import Data.List.Split
import Data.Char
import Text.Regex.PCRE

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
isValid passport = hasValidFields passport requiredFields

hasValidFields:: Passport -> [String] -> Bool
hasValidFields _ [] = True
hasValidFields passport (f:fs)
    | validate f (lookup f passport) == False = False
    | otherwise = hasValidFields passport fs

validate:: String -> Maybe String -> Bool
validate _ (Nothing) = False
validate f (Just(value))
    | f == "byr" = value `isIntInRange` (1920,2002)
    | f == "iyr" = value `isIntInRange` (2010,2020)
    | f == "eyr" = value `isIntInRange` (2020,2030)
    | f == "hgt" = validateHeight value
    | f == "hcl" = value =~ "#[0-9a-f]{6}"
    | f == "ecl" = value =~ "(amb)|(blu)|(brn)|(gr[ny])|hzl|oth"
    | f == "pid" = value `isNdigit` 9
    | otherwise = True

validateHeight:: String -> Bool
validateHeight h
    | h =~ "\\d\\din" = isIntInRange digits (59,76)
    | h =~ "\\d\\d\\dcm" = (digits) `isIntInRange` (150,193)
    | otherwise = False
    where
        digits = filter isDigit h

isIntInRange:: String -> (Int, Int) -> Bool
isIntInRange x (lower, upper)
    | length x /= length (filter isDigit x) = False
    | otherwise = lower <= n  && n <= upper
    where
        n = read x :: Int


isNdigit:: String -> Int -> Bool
isNdigit x n 
    | length x /= n = False
    | otherwise = length (filter isDigit x) == n
