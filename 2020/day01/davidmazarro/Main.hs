module Main where

main :: IO ()
main = do
  entries <- readFile "input.txt"
  putStrLn $ show $ prodTwoEntriesSum2020 $ map read $ lines entries
  putStrLn $ show $ prodThreeEntriesSum2020 $ map read $ lines entries

prodTwoEntriesSum2020 :: [Int] -> Int
prodTwoEntriesSum2020 entries =
  head [ x * y | x <- entries, y <- entries, x + y == 2020 ]
           
prodThreeEntriesSum2020 :: [Int] -> Int
prodThreeEntriesSum2020 entries =
  head [ x * y * z | x <- entries, y <- entries, z <- entries, x + y + z == 2020 ]
