module Main where

import Data.Char (isDigit, isHexDigit)
import Data.Foldable (foldl')
    
data Passport = Passport
  { birthYear :: Maybe Int,
    issueYear :: Maybe Int,
    expirationYear :: Maybe Int,
    height :: Maybe String,
    hairColor :: Maybe String,
    eyeColor :: Maybe String,
    passportID :: Maybe String,
    countryID :: Maybe String
  }

emptyPassport :: Passport
emptyPassport = Passport Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
              
main :: IO ()
main = do
  entries <- readFile "input.txt"
  let passports = (map parse . separateRawPassports . lines) entries
  putStrLn "*** 1st star ***"
  putStrLn $ show $ length $ filter (== True) $ map validateStar1 passports
  putStrLn "*** 2nd star ***"
  putStrLn $ show $ length $ filter (== True) $ map validateStar2 passports

separateRawPassports :: [String] -> [[String]]
separateRawPassports rawPassports = go [] rawPassports
    where separate = span (/= "")
          go :: [[String]] -> [String] -> [[String]]
          go l raw = 
              if raw == []
              then l
              else let (x,rest) = separate $ tail raw in
                   (concat $ map words $ x):(go l $ rest)
           
parse :: [String] -> Passport
parse = foldl' parseField emptyPassport

parseField :: Passport -> String -> Passport
parseField passport str =
    let (fieldName, fieldValue) = span (/= ':') str in
    if fieldName == "byr" then passport { birthYear = Just $ read $ tail fieldValue } else
    if fieldName == "iyr" then passport { issueYear = Just $ read $ tail fieldValue } else
    if fieldName == "eyr" then passport { expirationYear = Just $ read $ tail fieldValue } else
    if fieldName == "hgt" then passport { height = Just $ tail fieldValue } else
    if fieldName == "hcl" then passport { hairColor = Just $ tail fieldValue } else
    if fieldName == "ecl" then passport { eyeColor = Just $ tail fieldValue } else
    if fieldName == "pid" then passport { passportID = Just $ tail fieldValue } else
    if fieldName == "cid" then passport { countryID  = Just $ tail fieldValue } else
    passport

validateStar1 :: Passport -> Bool
validateStar1 passport =
    (\x -> if x == Nothing then False else True) $
    birthYear passport >>
    issueYear passport >>
    expirationYear passport >>
    height passport >>
    hairColor passport >>
    eyeColor passport >>
    passportID passport

validateStar2 :: Passport -> Bool
validateStar2 passport =
    let result = maybeValidateStar2 passport in
    case result of
      (Just res) -> res
      Nothing -> False 
               
maybeValidateStar2 :: Passport -> Maybe Bool
maybeValidateStar2 passport =
    do
      byr <- birthYear passport
      iyr <- issueYear passport
      eyr <- expirationYear passport
      hgt <- height passport
      hcl <- hairColor passport
      ecl <- eyeColor passport
      pid <- passportID passport
      let validation = and [(byr >= 1920 && byr <= 2002),
                            (iyr >= 2010 && iyr <= 2020),
                            (eyr >= 2020 && eyr <= 2030),
                            (validateHeight hgt),
                            (validateHairColor hcl),
                            (validateEyeColor ecl),
                            (length pid == 9 && all isDigit pid)]
      return validation

validateHeight :: String -> Bool
validateHeight hgt
    | metric == "cm" = numValue >= 150 && numValue <= 193
    | metric == "in" = numValue >= 59 && numValue <= 76
    | otherwise = False
    where (value, metric) = span isDigit hgt
          numValue = read value :: Int

validateHairColor :: String -> Bool
validateHairColor hcl =
    let value = tail hcl in
    head hcl == '#' && length value == 6 && all isHexDigit value

--amb blu brn gry grn hzl oth
validateEyeColor :: String -> Bool
validateEyeColor ecl =
    any (ecl ==) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
