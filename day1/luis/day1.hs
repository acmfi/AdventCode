module Main where

import Control.Monad (forever, when)
import System.Exit (exitSuccess)
import System.IO (hGetLine, hIsEOF, stdin)
import Data.Char (digitToInt)

listRshift :: [a] -> [a]
listRshift xs = last xs : init xs

shiftNTimes :: [a] -> [a]
shiftNTimes xs = iterate listRshift xs !! n
  where n = div (length xs) 2

repeatedSum :: ([Int] -> [Int]) -> [Int] -> Int
repeatedSum f = sum . zipWith' . listTuple
  where listTuple xs = (xs, f xs)
        zipWith' (xs, ys) = zipWith p xs ys
        p x y = if x == y then x else 0


main :: IO ()
main = forever $ do
  weAreDone <- hIsEOF stdin
  when weAreDone exitSuccess
  -- otherwise, proceed
  line <- hGetLine stdin
  putStrLn . show . repeatedSum listRshift  . map digitToInt $ line
  putStrLn . show . repeatedSum shiftNTimes . map digitToInt $ line
