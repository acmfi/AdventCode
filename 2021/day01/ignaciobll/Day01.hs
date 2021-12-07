module Day01 where

data DepthVariance = Increased | Decreased | NoChange deriving (Show, Eq)

calculateDepthVariance :: Ord a => a -> a -> DepthVariance
calculateDepthVariance x y
    | x < y = Increased
    | x > y = Decreased
    | otherwise = NoChange

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

star1 :: [Int] -> Int
star1 = count Increased . reduce
  where
    reduce :: [Int] -> [DepthVariance]
    reduce [] = []
    reduce [_] = []
    reduce (x : y : xs) = (calculateDepthVariance x y) : (reduce (y : xs))

star2 :: [Int] -> Int
star2 = count Increased . reduce . mapWindow
  where
    mapWindow :: [Int] -> [Int]
    mapWindow [] = []
    mapWindow [_] = []
    mapWindow (_ : _ : []) = []
    mapWindow (x : y : z : xs) = (x + y + z) : (mapWindow (y : z : xs))

    reduce :: [Int] -> [DepthVariance]
    reduce [] = []
    reduce [_] = []
    reduce (x : y : xs) = (calculateDepthVariance x y) : (reduce (y : xs))
