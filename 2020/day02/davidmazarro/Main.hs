module Main where

import Data.List (elemIndices)
    
main :: IO ()
main = do
  entries <- readFile "input.txt"
  putStrLn "*** 1st star ***"
  putStrLn $ show $ countValidPasswords checkLimitOnPass $ lines entries
  putStrLn "*** 2nd star ***"
  putStrLn $ show $ countValidPasswords checkLimitOnPass2 $ lines entries
  

countValidPasswords :: ((Int, Int) -> Char -> String -> Bool) -> [String] -> Int
countValidPasswords checker = length . (filter (== True)) . (map $ isValidPassword checker)

isValidPassword :: ((Int, Int) -> Char -> String -> Bool) -> String -> Bool
isValidPassword checker pass = checker limitRange limitChar actualPass
    where splitPass = words pass
          -- Here, fmap is used to apply a function just to the second element of the pair
          -- dropping the dash from the character limit of passwords that was left by span 
          limitRange = applyTuple read $ fmap (tail) $ span (/= '-') $ splitPass !! 0
          limitChar = head $ splitPass !! 1
          actualPass = splitPass !! 2

checkLimitOnPass :: (Int, Int) -> Char -> String -> Bool
checkLimitOnPass range char pass = inRange range $ length $ elemIndices char pass
    where inRange :: (Int, Int) -> Int -> Bool
          inRange (lb,ub) x  =  lb <= x && x <= ub

checkLimitOnPass2 :: (Int, Int) -> Char -> String -> Bool
checkLimitOnPass2 (a,b) char pass = ((\(x,y) -> x `xor` y) $ applyTuple (== char) chars)
    where chars = (pass !! (a-1), pass !! (b-1))
          xor = \x y -> not x && y || x && not y

applyTuple :: (a -> b) -> (a, a) -> (b, b)
applyTuple f (a, b) = (f a, f b)
