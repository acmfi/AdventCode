module Main where

import System.IO (getContents)
import Data.List (sort)

checksum :: ([Integer] -> Integer) -> [[Integer]] -> Integer
checksum f = sum . map f

difference :: [Integer] -> Integer
difference xs = maximum xs - minimum xs

readInts :: [[String]] -> [[Integer]]
readInts = map $ map toInt
  where toInt x = read x :: Integer -- unsafe... but easy (we should use Maybe)

divisible :: [Integer] -> Integer
divisible xs = head . filter (/= 1) . map f $ zs
  where ys = sort xs
        zs = reverse ys
        f x = foldr (\y acc -> if mod x y == 0 then div x y else acc) 0 ys
        

main :: IO ()
main = do
  input <- getContents
  putStrLn . show . checksum difference . readInts . map words . lines $ input
  putStrLn . show . checksum divisible  . readInts . map words . lines $ input
