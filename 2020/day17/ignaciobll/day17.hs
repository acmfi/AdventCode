#!/usr/bin/env stack
-- stack --resolver lts-16.24 script --package containers

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M

type Point3D = (Int, Int, Int)
type Point4D = (Int, Int, Int, Int)

data Status = Active | Inactive deriving (Show, Eq, Ord)

class Point a where
  neighBours :: a -> [a]

instance Point Point3D where
  neighBours = neighBours3d

instance Point Point4D where
  neighBours = neighBours4d


main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn $ "Star 1: " ++ (show $ star1 6 input)
  putStrLn $ "Star 2: " ++ (show $ star2 6 input)

parseInput :: String -> Map Point3D (Status, Int)
parseInput =
  M.fromList . fmap (\p -> (p, (Active, 0))) . fmap fst . filter ((== '#') . snd) . indexed
 where
  indexed =
    (id =<<)
      . fmap (\(x, l) -> fmap (\(y, c) -> ((x, y, 0), c)) $ zip [0 ..] l)
      . zip [0 ..] -- pair with x coordinate
      . lines

star1 :: (Ord a, Point a) => Int -> Map a (Status, Int) -> Int
star1 n m = length $ iterate step m !! n

star2 :: Int -> Map Point3D (Status, Int) -> Int
star2 n m = star1 n $ M.mapKeysMonotonic addDimension m

step :: (Ord a, Point a) => Map a (Status, Int) -> Map a (Status, Int)
step = getActives . tickAllSpace

getActives :: Map a (Status, Int) -> Map a (Status, Int)
getActives = M.filter ((== Active) . fst) . M.map rule
 where
  rule :: (Status, Int) -> (Status, Int)
  rule (Active  , 2) = (Active, 0)
  rule (Active  , 3) = (Active, 0)
  rule (Inactive, 3) = (Active, 0)
  rule (_       , _) = (Inactive, 0)

tickAllSpace :: (Point a, Ord a) => Map a (Status, Int) -> Map a (Status, Int)
tickAllSpace m = M.foldrWithKey' (\k _ mAcc -> tickNeigbours k mAcc) m m

tickNeigbours :: (Point a, Ord a) => a -> Map a (Status, Int) -> Map a (Status, Int)
tickNeigbours p m =
  foldr (\p' -> M.insertWith (\_ (s, c) -> (s, c + 1)) p' (Inactive, 1)) m $ neighBours p

neighBours3d :: Point3D -> [Point3D]
neighBours3d (x, y, z) =
  [ (x', y', z') | x' <- near x, y' <- near y, z' <- near z, (x', y', z') /= (x, y, z) ]
  where near n = [n - 1, n, n + 1]

neighBours4d :: Point4D -> [Point4D]
neighBours4d (x, y, z, t) =
  [ (x', y', z', t')
  | x' <- near x
  , y' <- near y
  , z' <- near z
  , t' <- near t
  , (x', y', z', t') /= (x, y, z, t)
  ]
  where near n = [n - 1, n, n + 1]

addDimension :: Point3D -> Point4D
addDimension (x, y, z) = (x, y, z, 0)
