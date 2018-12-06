import Data.Map.Strict (toList, fromListWith)
import Data.List (sortBy)

star1 :: [(Int, Int)] -> Int
star1 s = maximum $ map snd (frequency $ map (\y -> nearest y s)  [(a,b) | a <- [(down fst s)..(up snd s)], b <- [(down snd s)..(up snd s)]])
  where up = get maximum
        down = get minimum

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

nearest :: (Int, Int) -> [(Int, Int)] -> Maybe (Int,Int)
nearest x s = if 1 < length (filter (\q -> q == e) (e:elements)) then Nothing else Just (snd e)
  where (e:elements) = sortBy (\(a,_) (b,_) -> compare a b) $ map (\y -> (manhattan x y, y)) s

manhattan :: (Int, Int) -> (Int,Int) -> Int
manhattan (x,y) (a,b) = (abs $ x-a) + (abs $ y-b)

star2 :: [(Int, Int)] -> Int
star2 s = foldl (\n y -> if sum (map (manhattan y) s) <10000 then n+1 else n) 0 [(a,b) | a <- [(down fst s)..(up snd s)], b <- [(down snd s)..(up snd s)]]
  where up = get maximum
        down = get minimum

get :: ([a] -> c) -> (b -> a) -> [b] -> c
get a b s = a $ map b s

main :: IO()
main = do
  text <- readFile "input"
  let coord = Prelude.map (\x -> read $ '(':x++")") $ lines text :: [(Int,Int)]
  putStrLn $ show $ star1 coord
  putStrLn $ show $ star2 coord
