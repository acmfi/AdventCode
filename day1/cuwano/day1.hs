sum' :: (Num a, Eq a) => [a] -> a
sum' xs@(x:_) = megasum xs x

megasum :: (Num a, Eq a) => [a] -> a -> a
megasum [x] y
  | x == y = x
  | otherwise = 0
megasum (x:y:xs) z
  | x == y = x + megasum (y:xs) z
  | otherwise = megasum (y:xs) z
