#!/usr/bin/env stack
-- stack --resolver lts-16.24 script --package split
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (sort)
import Data.List.Split (chunksOf)
import Data.Maybe (listToMaybe)

main :: IO ()
main = do
  input <- fmap read . lines <$> getContents :: IO [Int]
  putStrLn $ "Star 1: " ++ show (star1 25 input)
  putStrLn $ "Star 1: " ++ show (star2 input (star1 25 input))

star1 :: Int -> [Int] -> Maybe Int
star1 n xs = go preamble sequence
  where
    preamble = take n xs
    sequence = drop n xs

    sums l = [ x + y | x <- l, y <- l]

    go :: [Int] -> [Int] -> Maybe Int
    go pre [] = Nothing
    go pre (s:seq)
      | s `elem` sums pre = go (tail pre ++ [s]) seq
      | otherwise = Just s


star2 :: [Int] -> Maybe Int -> Maybe Int
star2 xs Nothing = Nothing
star2 xs (Just n) = fmap (\(s, _) -> head (sort s) + last (sort s)) $             -- Result (sum of head and last)
                    listToMaybe $ filter (\(_, total) -> total == n) $ -- Get the first set that sums N
                    map (\(s :: [Int]) -> (s, sum s)) $ sets xs                  -- Calculate all sets and their sums


sets :: [Int] -> [[Int]]
sets [] = []
sets xs = (fmap (flip chunksOf xs) [2..length xs] >>= id) ++ sets (tail xs)
