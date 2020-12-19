#!/usr/bin/env stack
-- stack --resolver lts-16.24 script --package containers --package split

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntM

import           Data.List.Split (splitOn)
import           Data.Bits (setBit, clearBit, testBit)


data Stm = Mask String | Mem Int Int deriving (Show, Eq)

main :: IO ()
main = do
  stms <- parseInput <$> readFile "input.txt"
  putStrLn $ "Star 1: " ++ (show $ star1 stms)
  putStrLn $ "Star 2: " ++ (show $ star2 stms)

star1 :: [Stm] -> Int
star1 stms = sum $ go (take 32 $ repeat 'X') IntM.empty stms
  where
    go :: String -> IntMap Int -> [Stm] -> IntMap Int
    go m im [] = im
    go m im (Mask newMask: stms) = go newMask im stms
    go m im (Mem addr val: stms) = go m (IntM.insert addr (mask m val) im) stms

indexed :: [a] -> [(Int, a)]
indexed = reverse . zip [0..] . reverse

mask :: String -> Int -> Int
mask m n = foldr bitMask n $ indexed m
  where
    bitMask :: (Int, Char) -> Int -> Int
    bitMask (_, 'X') n = n
    bitMask (i, '1') n = n `setBit` i
    bitMask (i, '0') n = n `clearBit` i

star2 :: [Stm] -> Int
star2 stms = sum $ go "" IntM.empty stms
  where
    go :: String -> IntMap Int -> [Stm] -> IntMap Int
    go m im [] = im
    go m im (Mask newMask: stms) = go newMask im stms
    go m im (Mem addr val: stms) = go m (insertMulti val im (mask2 m addr)) stms

    insertMulti :: Int -> IntMap Int -> [Int] -> IntMap Int
    insertMulti val = foldr (flip IntM.insert val)

mask2 :: String -> Int -> [Int]
mask2 m n = fmap bin2int $ combine [[]] $ fmap (charList n) $ (indexed m)
  where
    charList :: Int -> (Int, Char) -> [Int]
    charList n (i, '0') = [if n `testBit` i then 1 else 0]
    charList _ (_, '1') = [1]
    charList _ (_, 'X') = [1,0]

bin2int :: [Int] -> Int
bin2int xs = foldr (\(i, b) n -> n + (2^i * b)) 0 (indexed xs)

combine :: [[Int]] -> [[Int]] -> [[Int]]
combine acc []       = acc
combine acc (xs:xss) = combine (go acc xs) xss
  where
    go :: [[Int]] -> [Int] -> [[Int]]
    go list ends = id =<< fmap (\l -> fmap (\e -> l ++ [e]) ends) list


parseInput :: String -> [Stm]
parseInput = fmap parseStm . lines
  where
    parseStm :: String -> Stm
    parseStm stm = case splitOn " = " stm of
                   ["mask", m] -> Mask m
                   [addr, n] -> Mem (parseAddr addr) (read n)

    parseAddr = read . init . last . splitOn "["
