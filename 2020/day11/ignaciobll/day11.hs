#!/usr/bin/env stack
-- stack --resolver lts-16.24 script --package matrix

import           Data.Foldable                  ( toList )

import           Data.Matrix                    ( Matrix )
import qualified Data.Matrix                   as M

import           Data.Maybe                     ( catMaybes
                                                , maybeToList
                                                , listToMaybe
                                                )

(!?) :: Matrix a -> (Int, Int) -> Maybe a
(!?) = flip $ uncurry M.safeGet

data Seat = Floor | Empty | Occupied deriving (Eq)

instance Show Seat where
  show Floor    = "."
  show Empty    = "L"
  show Occupied = "#"

main :: IO ()
main = do
  input <- parseInput <$> getContents
  putStrLn $ "Star 1: " ++ (show $ star1 input)
  putStrLn $ "Star 2: " ++ (show $ star2 input)

star1 :: Matrix Seat -> Int
star1 m = count (== Occupied) . last $ fixpoint step1 m

fixpoint :: Eq a => (a -> a) -> a -> [a]
fixpoint f a = a : (fmap snd $ takeWhile (\(a, b) -> a /= b) $ zip s s')
 where
  s  = iterate f a
  s' = tail s

step1 :: Matrix Seat -> Matrix Seat
step1 m = M.mapPos (\pos seat -> seatStep 4 seat (neighbours pos)) m
  where neighbours = catMaybes . fmap (m !?) . neighbourPos1

neighbourPos1 :: (Int, Int) -> [(Int, Int)]
neighbourPos1 (a, b) = [ (a + x, b + y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0) ]

star2 :: Matrix Seat -> Int
star2 m = count (== Occupied) . last $ fixpoint step2 m

step2 :: Matrix Seat -> Matrix Seat
step2 m = M.mapPos (\pos seat -> seatStep 5 seat (neighbours pos)) m
 where
  neighbours = (>>= id) . fmap fstSeat . neighbourPos2 (M.ncols m) (M.nrows m)
  fstSeat :: [(Int, Int)] -> [Seat]
  fstSeat = -- Takes the first seat distinct of Floor in given collection of points. Empty by default
    maybeToList . listToMaybe . dropWhile (== Floor) . catMaybes . fmap (m !?)

neighbourPos2 :: Int -> Int -> (Int, Int) -> [[(Int, Int)]]
neighbourPos2 cols rows (x, y) =
  [ reverse [ (x, y') | y' <- [1 .. y - 1] ] -- Left
  , id [ (x - x', y - y') | y' <- [1 .. y - 1], x' <- [1 .. x - 1], x' == y' ] -- Above Left
  , reverse [ (x', y) | x' <- [1 .. x - 1] ] -- Above
  , [ (x - x', y + y')
    | x' <- [1 .. rows]
    , y' <- [1 .. cols]
    , x' == y'
    , x - x' >= 1
    , y + y' <= cols
    ] -- Above Right
  , id [ (x, y') | y' <- [y + 1 .. cols] ] -- Right
  , [ (x' + x, y' + y)
    | y' <- [1 .. rows]
    , x' <- [1 .. cols]
    , x' == y'
    , x' + x <= rows
    , y' + y <= cols
    ] -- Below Right
  , id [ (x', y) | x' <- [x + 1 .. rows] ] -- Below
  , [ (x + x', y - y')
    | x' <- [1 .. rows]
    , y' <- [1 .. cols]
    , x' == y'
    , x + x' <= rows
    , y - y' >= 1
    ] -- Below Left
  ]


parseInput :: String -> Matrix Seat
parseInput = M.fromLists . fmap (fmap parseSeat) . lines

parseSeat :: Char -> Seat
parseSeat 'L' = Empty
parseSeat '.' = Floor
parseSeat '#' = Occupied

seatStep :: Int -> Seat -> [Seat] -> Seat
seatStep _ Floor _ = Floor
seatStep _ Empty near | count (== Occupied) near == 0 = Occupied
                      | otherwise                     = Empty
seatStep n Occupied near | count (== Occupied) near >= n = Empty
                         | otherwise                     = Occupied

count :: Foldable f => (a -> Bool) -> f a -> Int
count pred = length . filter pred . toList
