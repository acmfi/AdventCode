import Data.List
import Data.Maybe
import Data.Fixed

-- Part 1
spiral :: Int -> Int
spiral number = distance number (findSq number)

distance :: Int -> Int -> Int
distance num sq = dist1 + (dist2 num sq (root-1))
  where root = round $ sqrt $ fromIntegral sq
        dist1 = (root - 1)`div` 2
        dist2 num sq rootM
          | num < (sq - rootM) = dist2 num (sq - rootM) rootM
          | otherwise = abs (num - (sq - dist1))

-- Part 2

-- find (\x -> (spiral2 x) > 277678) [1..]
-- >> Just 59
-- spiral2 59
-- >> 279138

spiral2 :: Int -> Int
spiral2 num
  | num <= 0 = 0
  | num == 1 = 1
  | num == 2 = 1
  | num == 4 = 4
  | num == 6 = 10
  | corner num  = (spiral2 $ num - 1) + (spiral2 $ findCor num sq root) + (if (num == sq) then (spiral2 (previous num)) else (0))
  | (num - 1) == (findSq $ num - 1) = spiral2 (num - 1) + spiral2 (next (num - 1)) 
  | corner (previous num) = spiral2 (previous num) + (spiral2 (previous (previous num))) + (spiral2 (findCor (previous num) sq root)) + (spiral2 $ next $ findCor (previous num) sq root)
  | corner (next num) = (spiral2 $ previous num) + (spiral2 $ findCor (next num) sq root) + (spiral2 ((findCor (next num) sq root) - 1)) + (if ((num + 1) == sq) then (spiral2 ((root - 2)^2 + 1)) else (0))
  | otherwise = (spiral2 $ num - 1) + (spiral2 $ down num sq root) + (spiral2 $ next $ down num sq root) + (spiral2 $ previous $ down num sq root)
  where sq = findSq num
        root = round $ sqrt $ fromIntegral sq
        corner :: Int -> Bool
        corner num
          | num == 0 = False
          | ((num - ((root-2)^2)) `mod` (root-1)) == 0 = True
          | otherwise = False
  

findCor :: Int -> Int -> Int -> Int                
findCor num sq root
  | num == sq = num - (4 * (root - 1))
  | num == (sq - (root - 1)) = num - (3 * (root - 1)) - (root - 3)
  | num == (sq - 2 * (root - 1)) = num - (2 * (root - 1)) - (2 * (root - 3))
  | otherwise = num - (root - 1) - (3 *(root - 3))

-- Previous 10 = 25 
previous :: Int -> Int
previous num
  | (num - 1) == (findSq $ num-1) = sq
  | num == sq = ((root - 2)^2) + 1
  | otherwise = num - 1
  where sq = findSq num
        root = round $ sqrt $ fromIntegral sq

-- next 25 = 10  
next :: Int -> Int
next num
  | num == sq = (root - 2)^2 + 1
  | otherwise = num + 1
  where sq = findSq num
        root = round $ sqrt $ fromIntegral sq
  
down :: Int -> Int -> Int -> Int  
down num sq root = down' num sq (root - 1) ((root-2)^2) (root-3)

down' :: Int -> Int -> Int -> Int -> Int -> Int
down' num sq rootM sqB rootMB
  | num < (sq - rootM) = down' num (sq - rootM) rootM (sqB - rootMB) rootMB
  | otherwise = sqB - (sq - num - 1)
                
-- Primer cuadrado de un impar mayor que el número
findSq :: Int -> Int
findSq number = fromMaybe 0 (find (\x -> (odd x) && ((round $ ((sqrt $ fromIntegral  x) * 10) `mod'` 10) == 0)) [number..])


