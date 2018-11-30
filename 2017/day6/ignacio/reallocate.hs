{-# LANGUAGE BangPatterns #-}

import Data.List (elemIndex, elemIndices)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Either

{- BASIC FUNCTIONS -}
    
update :: (a -> a) -> Int -> [a] -> [a]
update f index xs = pre ++ (f v):pos
    where (pre, v:pos) = splitAt index xs

zero = \_ -> 0
                         
step :: [Int] -> [Int]
step xs = zipWith (+) (update zero maxIndex xs) (dis)
    where max = maximum xs
          maxIndex = fromJust $ elemIndex max xs
          dis = rotate (length xs - maxIndex - 1) (distribute max xs)

rotate n xs = bs ++ as where (as, bs) = splitAt n xs

distribute :: Int -> [a] -> [Int]
distribute v xs = zipWith (+) base carr
    where len = length xs
          base = take len . repeat $ div v len
          carr = take len $ (take (mod v len) $ repeat 1) ++ repeat 0


{- COMPOSED FUNCTIONS -}

duplicateState :: [Int] -> [Int]
duplicateState state0 = dup ist
    where ist = iterate step state0


dup :: Ord a => [a] -> a
dup xs = dup' xs Set.empty
    where dup' (x:xs) s = if Set.member x s then
                              x else dup' xs (Set.insert x s)


firstDupPositions :: [Int] -> [Int]
firstDupPositions state0 = elemIndices (duplicateState state0) its
    where its = iterate step state0


{- TEST INPUTS -}

test1 :: [Int]
test1 = [0,2,7,0]

input :: [Int]    
input = [0, 5, 10, 0, 11, 14, 13, 4, 11, 8, 8, 7, 1, 4, 12, 11]


{- MAIN -}

main :: IO ()
main = do
  putStrLn . show $ take 2 $ firstDupPositions input
  putStrLn . show $ (\(a:b:[]) -> b - a) $ take 2 $ firstDupPositions input

