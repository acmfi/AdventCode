module Main where

import Debug.Trace
    
main :: IO ()
main = do
  input <- readFile "input.txt"
  let inputLines = lines input
  let earliest = read $ inputLines !! 0 :: Int
  let buses = inputLines !! 1
  let earliestBuses = map (\(x, y) -> (x, head $ dropWhile (< earliest) y)) $
                      (repeatingTimestamps . parseOperatingBuses) buses
  let (busID, timestamp) = foldl
                           (\t1@(_, y1) t2@(_, y2) -> if y1 < y2 then t1 else t2)
                           (head earliestBuses)
                           earliestBuses
  let firstStar = busID * (timestamp - earliest)
  putStrLn "*** 1st star ***"
  putStrLn $ show firstStar

  let secondStar = (chineseRemainder . getConstraints) buses
  putStrLn "*** 2nd star ***"
  putStrLn $ show secondStar

parseOperatingBuses :: String -> [Int]
parseOperatingBuses input = map read $ filter (/= "x") $ splitOnChar ',' input

repeatingTimestamps :: [Int] -> [(Int, [Int])]
repeatingTimestamps = map (\x -> (x, zipWith (*) [1..] $ repeat x))

-- Blatantly copy-pasted from the Internet because I didn't want
-- to import Data.List.Split which is not from the base library
splitOnChar :: Char -> String -> [String]
splitOnChar x = foldr (\c (s : ss) -> if c == x then "" : s : ss else (c : s) : ss) [""]

-- Returns the list of the required offsets from t and the bus IDs
getConstraints :: String -> [(Integer, Integer)]
getConstraints input = map (\(x,y) -> (x, read y)) $
                       filter (\(_,y) -> y /= "x") $
                       zipWith (,) [0..] $ splitOnChar ',' input
                
-- Blatantly copy-pasted from Rosetta Code because I profoundly hate number theory
egcd :: Integer -> Integer -> (Integer, Integer)
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
    (s, t) = egcd b r
    (q, r) = a `quotRem` b
 
modInv :: Integer -> Integer -> Integer
modInv a b =
  case egcd a b of
    (x, y)
      | a * x + b * y == 1 -> x
      | otherwise -> error "Shouldn't have failed"

chineseRemainder :: [(Integer, Integer)] -> Integer
chineseRemainder constraints = (sum $ zipWith (*) a $ zipWith (*) b b') `mod` modPI
    where
      modulii = map snd constraints
      modPI = product modulii
      a = map fst constraints
      b = map (modPI `div`) modulii
      b' = zipWith modInv b modulii
