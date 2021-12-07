{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day06 where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

testInput :: Text
testInput = "3,4,3,1,2"

newtype FishState = FishState (Int, Int, Int, Int, Int, Int, Int, Int, Int) deriving (Show, Eq)

parse :: Text -> [Int]
parse = fmap (read . T.unpack) . T.splitOn ","

stepN :: Int -> FishState -> FishState
stepN 0 fish = fish
stepN n (FishState (_0, _1, _2, _3, _4, _5, _6, _7, _8)) =
    stepN (n - 1) (FishState (_1, _2, _3, _4, _5, _6, _7 + _0, _8, _0))

placeFish :: [Int] -> FishState
placeFish = toFishState . M.fromListWith (+) . (emptyFishState ++) . fmap (,1)
  where
    emptyFishState = [(n, 0) | n <- [0 .. 8]]
    toFishState :: IntMap Int -> FishState
    toFishState m = case fmap snd (M.toAscList m) of
        [_0, _1, _2, _3, _4, _5, _6, _7, _8] -> FishState (_0, _1, _2, _3, _4, _5, _6, _7, _8)
        _ -> FishState (0, 0, 0, 0, 0, 0, 0, 0, 0)

count :: FishState -> Int
count (FishState (_0, _1, _2, _3, _4, _5, _6, _7, _8)) =
    _0 + _1 + _2 + _3 + _4 + _5 + _6 + _7 + _8

star1 :: [Int] -> Int
star1 = count . stepN 80 . placeFish

star2 :: [Int] -> Int
star2 = count . stepN 256 . placeFish

main :: IO ()
main = do
    input <- parse <$> T.getContents
    -- input <- parse <$> T.readFile "input"
    putStrLn $ "Test 1: " ++ show (star1 $ parse testInput)
    putStrLn $ "Star 1: " ++ show (star1 input)
    putStrLn $ "Test 2: " ++ show (star2 $ parse testInput)
    putStrLn $ "Star 2: " ++ show (star2 input)
