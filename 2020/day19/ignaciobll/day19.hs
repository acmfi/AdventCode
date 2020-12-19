#!/usr/bin/env stack
-- stack --resolver lts-16.24 script --package containers --package split

import           Data.List                      ( elem )
import           Data.List.Split                ( splitOn )

import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IntM


type RuleId = Int
data RuleDef = Match Char | OneOf [RuleId] [RuleId] | Rules [RuleId] deriving (Show, Eq)
type Rule = (RuleId, RuleDef)

main :: IO ()
main = do
  (ruleMap, inputs) <- parseInput <$> getContents
  putStrLn $ "Star 1: " ++ (show $ star1 ruleMap inputs)
  putStrLn $ "star 1: " ++ (show $ star2 ruleMap inputs)


star1 :: IntMap RuleDef -> [String] -> Int
star1 rm = length . filter id . fmap (checkRule rm [0])

star2 :: IntMap RuleDef -> [String] -> Int
star2 = star1 . IntM.insert 8 rule8 . IntM.insert 11 rule11
 where
  rule8  = OneOf [42] [42, 8]
  rule11 = OneOf [42, 31] [42, 11, 31]

checkRule :: IntMap RuleDef -> [RuleId] -> String -> Bool
checkRule im []          []       = True
checkRule im _           []       = False
checkRule im []          _        = False
checkRule im (rID : ids) (c : cs) = case IntM.lookup rID im of
  Nothing            -> False
  Just (Match c'   ) -> c == c' && checkRule im ids cs
  Just (Rules rs   ) -> checkRule im (rs ++ ids) (c : cs)
  Just (OneOf r1 r2) -> checkRule im (r1 ++ ids) (c : cs) || checkRule im (r2 ++ ids) (c : cs)

parseInput :: String -> (IntMap RuleDef, [String])
parseInput src = (ruleMap, input)
 where
  [rulesSrc, inputSrc] = splitOn "\n\n" src

  rules                = parseRule <$> lines rulesSrc
  input                = lines inputSrc

  ruleMap              = IntM.fromList rules

  parseRule :: String -> (RuleId, RuleDef)
  parseRule r =
    let [idR, body] = splitOn ": " r
        ruleBody    = parseRuleBody body
    in  (read idR, ruleBody)

  parseRuleBody :: String -> RuleDef
  parseRuleBody src
    | '"' `elem` src = Match (src !! 1)
    | '|' `elem` src = (\[l, r] -> OneOf l r) $ fmap (fmap read . words) $ splitOn " | " src
    | otherwise      = Rules $ fmap read $ words src
