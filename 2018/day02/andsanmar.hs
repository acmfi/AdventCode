star1 :: [String] -> Integer
star1 s = fst checksum * snd checksum
  where checksum = foldl (\(a,b) (x,y) -> (a+ tobin x,b+ tobin y)) (0,0) (map reps s)

tobin :: Bool -> Integer
tobin True = 1
tobin False = 0

reps :: String -> (Bool, Bool)
reps l = foldl (\(a,b) y -> (a || nocs y == 2, b || nocs y == 3)) (False, False)  l
  where nocs k = length $ filter (==k) l

star2 :: [String] -> String
star2 l = foldl (\x y -> if length x > length y then x else y) "" (hl l)

hl :: (Ord a) => [[a]] -> [[a]]
hl [] = []
hl (x:xs) = map (ord_common x) xs ++ hl xs

ord_common :: (Ord a) => [a] -> [a] -> [a]
ord_common [] [] = []
ord_common _ [] = []
ord_common [] _ = []
ord_common (x:xs) (y:ys) = (if x == y then (x:) else id) (ord_common xs ys)

main :: IO()
main = do
  text <-  readFile "input"
  let ids = lines text
  putStrLn $ show $ star1 ids
  putStrLn $ show $ star2 ids
