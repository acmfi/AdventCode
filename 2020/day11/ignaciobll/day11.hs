#!/usr/bin/env stack
-- stack --resolver lts-16.24 script --package matrix


import           Data.Foldable                  ( toList )

import           Data.Matrix                    ( Matrix )
import qualified Data.Matrix                   as M

import           Data.Maybe                     ( catMaybes )

(!?) :: Matrix a -> (Int, Int) -> Maybe a
(!?) = flip $ uncurry M.safeGet

data Seat = Floor | Empty | Occupied deriving (Eq)

instance Show Seat where
  show Floor    = "."
  show Empty    = "L"
  show Occupied = "#"

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn $ "Star 1: " ++ (show $ star1 input)

star1 :: Matrix Seat -> Int
star1 m = count (== Occupied) . last $ fixpoint step1 m

fixpoint :: Eq a => (a -> a) -> a -> [a]
fixpoint f a = a : (fmap snd $ takeWhile (\(a, b) -> a /= b) $ zip s s')
 where
  s  = iterate f a
  s' = tail s

step1 :: Matrix Seat -> Matrix Seat
step1 m = M.mapPos (\pos seat -> seatStep seat (neighbours pos)) m
  where neighbours = catMaybes . fmap (m !?) . neighbourPos1

neighbourPos1 :: (Int, Int) -> [(Int, Int)]
neighbourPos1 (a, b) = [ (a + x, b + y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0) ]

star2 :: Matrix Seat -> Int
star2 m = count (== Occupied) . last $ fixpoint step2 m

step2 :: Matrix Seat -> Matrix Seat
step2 m = m

neighbourPos2 :: (Int, Int) -> [(Int, Int)]
neighbourPos2 = undefined

parseInput :: String -> Matrix Seat
parseInput = M.fromLists . fmap (fmap parseSeat) . lines

parseSeat :: Char -> Seat
parseSeat 'L' = Empty
parseSeat '.' = Floor
parseSeat '#' = Occupied

seatStep :: Seat -> [Seat] -> Seat
seatStep Floor _ = Floor
seatStep Empty near | count (== Occupied) near == 0 = Occupied
                    | otherwise                     = Empty
seatStep Occupied near | count (== Occupied) near >= 4 = Empty
                       | otherwise                     = Occupied

count :: Foldable f => (a -> Bool) -> f a -> Int
count pred = length . filter pred . toList
