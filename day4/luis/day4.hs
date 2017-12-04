module Main where

import System.IO (getContents)
import Data.List

checker :: [String] -> Bool
checker = (==) [] . filter (> 1) . map length . group . sort

noanagram :: [String] -> Bool
noanagram xs = (== length xs) . length . foldr f [] $ xs
  where f x acc = if g x acc == [] then x : acc else acc
        g x = filter $ (== sort x) . sort

passphrasses :: (a -> Bool) -> [a] -> Int
passphrasses f = length . filter id . map f

main :: IO ()
main = do
  input <- getContents
  putStrLn . show . passphrasses checker   . map words . lines $ input
  putStrLn . show . passphrasses noanagram . map words . lines $ input
