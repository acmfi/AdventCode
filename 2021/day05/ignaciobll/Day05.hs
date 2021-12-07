{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Day05 where

import Data.Text (unpack)
import NeatInterpolation (text)
import Text.RawString.QQ (r)
import Text.Regex.TDFA ((=~))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

testInput =
    unpack
        [text|
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
|]

newtype Line = Line ((Int, Int), (Int, Int)) deriving (Show, Eq)

parse :: String -> [Line]
parse input =
    let regex = [r|([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)|]
        lineFromMatch :: [String] -> Line
        lineFromMatch [_, a, b, c, d] = Line ((read a, read b), (read c, read d))
        lineFromMatch match = error $ show match
     in lineFromMatch <$> (=~ regex) input

isDiagonal :: Line -> Bool
isDiagonal (Line ((a, b), (c, d))) = not $ a == c || b == d

genPoints :: Line -> [(Int, Int)]
genPoints (Line ((a, b), (c, d)))
    | a == c = [(a, b') | b' <- [min b d .. max b d]]
    | b == d = [(a', b) | a' <- [min a c .. max a c]]
    | otherwise =
        zip
            ((if c < a then reverse else id) [min a c .. max a c])
            ((if d < b then reverse else id) [min b d .. max b d])

fillSpace :: [Line] -> Map (Int, Int) Int
fillSpace = M.fromListWith (+) . fmap (,1) . mconcat . fmap genPoints

star1 :: [Line] -> Int
star1 = M.size . M.filter (> 1) . fillSpace . filter (not . isDiagonal)

star2 :: [Line] -> Int
star2 = M.size . M.filter (> 1) . fillSpace

main :: IO ()
main = do
    input <- parse <$> getContents
    putStrLn $ "Test 1: " ++ show (star1 $ parse testInput)
    putStrLn $ "Star 1: " ++ show (star1 input)
    putStrLn $ "Test 2: " ++ show (star2 $ parse testInput)
    putStrLn $ "Star 2: " ++ show (star2 input)
