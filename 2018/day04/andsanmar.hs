import Data.Map.Strict as Map
import Data.List (sort)

parse_input :: [String] -> Map Int [Int]
parse_input s = parse_input' s 0 Map.empty

parse_input' :: [String] -> Int -> Map Int [Int] -> Map Int [Int]
parse_input' [] _ m = m
parse_input' (x:y:xs) n m = if bel "Guard" x then parse_input' (y:xs) (getnGuard x) m
                            else parse_input' xs n (Map.insert n ([a..b-1] ++ u) m)
  where a = getminute x
        b = getminute y
        bel = \q w -> elem q (words w)
        u = if Map.lookup n m == Nothing then [] else p
        Just p = Map.lookup n m
parse_input' _ _ m = m
        
getnGuard :: String -> Int
getnGuard s = read $ tail $ words s !! 3

getminute :: String -> Int
getminute s = read $ init $ snd $ Prelude.splitAt 3 $ words s !! 1

star1 :: Map Int [Int] -> Int
star1 m = (fst u) * (fst $ Prelude.foldl1 (\x y -> if snd y > snd x then y else x) (frequency $ snd u))
  where u = Prelude.foldl (getHours m) (0,[]) (Map.keys m)

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

getHours :: Ord a => Map a [a] -> (a, [a]) -> a -> (a, [a])
getHours m (a,b) k = if (length u) > (length b) then (k, u) else (a,b)
  where Just u = Map.lookup k m

star2 :: Map Int [Int] -> Int
star2 m = (fst u) * (fst $ snd u)
  where u = Prelude.foldl (getSnoozy m) (0,(0, 0)) (Map.keys m)

getSnoozy :: Ord a => Map a [a] -> (a, (a, Int)) -> a -> (a, (a, Int))
getSnoozy m (a,b) k = if snd j > snd b then (k, j) else (a,b)
  where j = foldl1 (\x y -> if snd x > snd y then x else y) (frequency u)
        Just u = Map.lookup k m

main :: IO()
main = do
  text <-  readFile "input"
  let table = parse_input $ sort $ lines text
  putStrLn $ show $ star1 table
  putStrLn $ show $ star2 table
