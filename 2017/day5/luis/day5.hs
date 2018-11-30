module Main where

import System.IO   (getContents)
import Data.Char   (digitToInt)
import Data.Either (isLeft, fromLeft)
import Data.Maybe  (fromJust, isJust)
import Data.Sequence

data State = State { jumps :: Seq Int, pos :: Int, step :: Int } deriving (Eq, Show)

initialState :: [Int] -> State
initialState xs = State { jumps = fromList xs, pos = 0, step = 0}

nextState :: (Int -> Int) -> Either State State -> Either State State
nextState _ (Left  state) = Left state
nextState f (Right state)
  | isJust offset = Right State { jumps = jumps', pos = pos state + offset', step = step state + 1 }
  | otherwise = Left state
  where offset  = jumps state !? pos state
        offset' = fromJust offset
        jumps'  = update (pos state) (f offset') (jumps state)

steps :: (Int -> Int) -> [Int] -> Int
steps f = step . fromLeft noState . until isLeft (nextState f) . Right . initialState 
  where noState = State {jumps = empty, pos = 0, step = 0}

readInts :: [String] -> [Int]
readInts = fmap toInt
  where toInt x = read x :: Int -- unsafe... but easy (we should use Maybe)

main :: IO ()
main = do
  input <- getContents
  putStrLn . show . steps f . readInts . lines $ input
  putStrLn . show . steps g . readInts . lines $ input
  where f = (+) 1
        g x = if x >= 3 then x - 1 else x + 1
