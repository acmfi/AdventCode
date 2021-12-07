module Day02 where

import Data.List.Extra

data Command = FORWARD Int | DOWN Int | UP Int deriving (Show, Read)

newtype Position = Position {getPosition :: (Int, Int)} deriving (Show)
newtype Aim = Aim {getAim :: Int} deriving (Show)
newtype State = State {getState :: (Position, Aim)} deriving (Show)

star1 :: [Command] -> Int
star1 = uncurry (*) . getPosition . foldl step start
  where
    start = Position (0, 0)

    step :: Position -> Command -> Position
    step (Position (h, v)) (FORWARD h') = Position (h + h', v)
    step (Position (h, v)) (DOWN v') = Position (h, v + v')
    step (Position (h, v)) (UP v') = Position (h, v - v')

star2 :: [Command] -> Int
star2 = uncurry (*) . getPosition . fst . getState . foldl step start
  where
    start = State (Position (0, 0), Aim 0)

    step :: State -> Command -> State
    step (State (Position (h, v), Aim a)) (FORWARD h') = State (Position (h + h', v + (h' * a)), Aim a)
    step (State (Position (h, v), Aim a)) (DOWN v') = State (Position (h, v), Aim (a + v'))
    step (State (Position (h, v), Aim a)) (UP v') = State (Position (h, v), Aim (a - v'))

parse :: String -> [Command]
parse = fmap (read . upper) . lines

main :: IO ()
main = do
    input <- parse <$> getContents
    putStrLn $ "Star 1: " ++ show (star1 input)
    putStrLn $ "Star 2: " ++ show (star2 input)
