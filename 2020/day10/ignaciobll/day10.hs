#!/usr/bin/env stack
-- stack --resolver lts-16.24 script

import Data.List (sort)

main :: IO ()
main = do
  input <- (fmap read) . lines <$> readFile "input.txt" :: IO [Int]
  putStrLn $ "Star 1: " ++ show (star1 input)


star1 :: [Int] -> Int
star1 = (\(o, t) -> o * t) . foldr count (1, 1) . joltDiff . sort
  where
    count :: Int -> (Int, Int) -> (Int, Int)
    count 1 (acc1, acc3) = (succ acc1, acc3)
    count 3 (acc1, acc3) = (acc1, succ acc3)
    count _          acc = acc

joltDiff :: [Int] -> [Int]
joltDiff [] = []
joltDiff xs@(_:xs') = zipWith (-) xs' xs
