#!/usr/bin/env stack
-- stack --resolver lts-16.24 script --package containers

import           Data.Char                      ( toUpper )

data Orientation = N | S | E | W | L | R | F deriving (Show, Eq, Read)

type Action = (Orientation, Int)

type Position = (Int, Int) -- N, E (-N = S, -E = W)

data Boat = Boat
  { getPosition :: Position
  , getOrientation :: Orientation
  } deriving (Show, Eq)

newtype WayPoint = WayPoint { getWPPosition :: Position } deriving (Show, Eq)

main :: IO ()
main = do
  input <- fmap parseAction . lines <$> getContents
  putStrLn $ "Star 1: " ++ show (star1 input)
  putStrLn $ "Star 2: " ++ show (star2 input)

star1 :: [Action] -> Int
star1 = distance (0, 0) . getPosition . foldr move (Boat (0, 0) E)

star2 :: [Action] -> Int
star2 = distance (0, 0) . getPosition . fst . foldl (flip moveWP) (Boat (0, 0) E, WayPoint (1, 10))

parseAction :: String -> Action
parseAction (o : n) = (read $ (toUpper o) : "", read n)

distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

move :: Action -> Boat -> Boat
move (N, k) (Boat (n, e) o) = Boat (n + k, e) o
move (S, k) (Boat (n, e) o) = Boat (n - k, e) o
move (E, k) (Boat (n, e) o) = Boat (n, e + k) o
move (W, k) (Boat (n, e) o) = Boat (n, e - k) o
move (F, k) (Boat (n, e) o) = move (o, k) (Boat (n, e) o)
move a      b               = rotate a b

rotate :: Action -> Boat -> Boat
rotate (_, 180) (Boat p o) = Boat p (inverse o)
rotate (R, 90 ) (Boat p o) = Boat p (right o)
rotate (L, 90 ) (Boat p o) = Boat p (left o)
rotate (a, 270) boat       = rotate (inverse a, 90) boat

inverse :: Orientation -> Orientation
inverse N = S
inverse S = N
inverse E = W
inverse W = E
inverse R = L
inverse L = R
inverse F = F

right :: Orientation -> Orientation
right N = E
right E = S
right S = W
right W = N

left :: Orientation -> Orientation
left E = N
left S = E
left W = S
left N = W

moveWP :: Action -> (Boat, WayPoint) -> (Boat, WayPoint)
moveWP (N, k) (boat, WayPoint (n, e)) = (boat, WayPoint (n + k, e))
moveWP (S, k) (boat, WayPoint (n, e)) = (boat, WayPoint (n - k, e))
moveWP (E, k) (boat, WayPoint (n, e)) = (boat, WayPoint (n, e + k))
moveWP (W, k) (boat, WayPoint (n, e)) = (boat, WayPoint (n, e - k))
moveWP (F, k) ((Boat (n, e) o), wp@(WayPoint (wpN, wpE))) =
  (Boat (n + (k * wpN), e + (k * wpE)) o, wp)
moveWP a b = rotateWP a b

rotateWP :: Action -> (Boat, WayPoint) -> (Boat, WayPoint)
rotateWP (_, 180) (Boat p o, WayPoint (wpN, wpE)) = (Boat p (inverse o), WayPoint (-wpN, -wpE))
rotateWP (R, 90 ) (Boat p o, WayPoint (wpN, wpE)) = (Boat p (right o), WayPoint (-wpE, wpN))
rotateWP (L, 90 ) (Boat p o, WayPoint (wpN, wpE)) = (Boat p (left o), WayPoint (wpE, -wpN))
rotateWP (a, 270) bwp                             = rotateWP (inverse a, 90) bwp
