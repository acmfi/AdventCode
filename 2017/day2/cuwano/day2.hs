checksum :: (Num a, Ord a) => [[a]] -> a
checksum = foldl (\acc x -> acc + sumRow x) 0

sumRow :: (Num a, Ord a) => [a] -> a
sumRow x = abs (largest x - smallest x)

largest :: (Num a, Ord a) => [a] -> a
largest (x:xs) = foldl (\acc x -> if x > acc then x else acc) x xs

smallest :: (Num a, Ord a) => [a] -> a
smallest (x:xs) = foldl (\acc x -> if x < acc then x else acc) x xs
