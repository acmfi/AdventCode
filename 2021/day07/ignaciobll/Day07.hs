{-# LANGUAGE OverloadedStrings #-}

module Day07 where

import NeatInterpolation

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.List (sort)

testInput = "16,1,2,0,4,2,7,1,2,14"

parse :: Text -> [Int]
parse = fmap (read . T.unpack) . T.splitOn ","

count2 :: [Int] -> Int
count2 ns = case foldr (\n (z, o) -> if n == 0 then (z + 1, o) else (z, o + 1)) (0, 0) ns of
    (z, o) -> if z > o then 0 else 1

fuelCostSimple :: Int -> Int -> Int
fuelCostSimple a b = abs $ a - b

fuelCostAggregated :: Int -> Int -> Int
fuelCostAggregated a b = ((a - b) ^ 2 + abs (a - b)) `div` 2

minimumFuel :: (Int -> Int -> Int) -> [Int] -> Int
minimumFuel fuelCost crabs =
    fst . minimum $
        [ (sum (fmap (fuelCost position) crabs), position)
        | position <- [minimum crabs .. maximum crabs]
        ]

star1 :: [Int] -> Int
star1 = minimumFuel fuelCostSimple

star2 :: [Int] -> Int
star2 = minimumFuel fuelCostAggregated

main :: IO ()
main = do
    input <- parse <$> T.readFile "input"
    putStrLn $ "Test 1: " ++ show (star1 $ parse testInput)
    putStrLn $ "Star 1: " ++ show (star1 input)
    putStrLn $ "Test 1: " ++ show (star2 $ parse testInput)
    putStrLn $ "Star 2: " ++ show (star2 input)
