module Main where

import Debug.Trace
import Data.Foldable (foldl')
    
main :: IO ()
main = do
  entries <- readFile "input.txt"
  let terrainMap = (generateMap . lines) entries
  putStrLn "*** 1st star ***"
  putStrLn $ show $ firstStar terrainMap
  putStrLn "*** 2nd star ***"
  putStrLn $ show $ secondStar terrainMap

firstStar :: [String] -> Int
firstStar terrainMap = numberOfTrees $ lookupPositions (0, 0) terrainMap (3, 1)

secondStar :: [String] -> Int
secondStar terrainMap =
    product $
    map numberOfTrees $
    fmap (lookupPositions (0,0) terrainMap) $ slopes
    where slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
            
-- Thanks to laziness, we can repeat indefinitely our terrain map
-- without that being a memory problem until evaluated
generateMap :: [String] -> [String]
generateMap = map (foldr (++) []) . map repeat

-- Returns a list of the positions that have been traversed
traverseMap :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)]
traverseMap (incX, incY) initPos limit =
    takeWhile (\(_,y) -> y < limit) $
    iterate (\(x, y) -> (x + incX, y + incY)) initPos

-- Returns the list of elements (free square or tree) under
-- the positions that have been traversed
lookupPositions :: (Int, Int) -> [String] -> (Int, Int) -> [Char]
lookupPositions initPos terrainMap moveFunc =
    map (\(x, y) -> (terrainMap !! y) !! x) gatheredPos
    where gatheredPos = traverseMap moveFunc initPos $ length terrainMap
    
numberOfTrees :: [Char] -> Int
numberOfTrees = (foldl' (+) 0) . (map valueOfPos)
          
valueOfPos :: Char -> Int
valueOfPos '.' = 0
valueOfPos '#' = 1
valueOfPos _ = error "The provided input was invalid."
