import Data.Map.Strict (toList, fromListWith)
import Data.List (sortBy)

star1 :: [(Int, Int)] -> Int
star1 s = maximum $ map snd $ frequency $ map (nearest s) (inbounds s)

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

nearest :: [(Int, Int)] -> (Int, Int) -> Maybe (Int,Int)
nearest s x = if 1 < length (filter (==e) (e:elements)) then Nothing else Just (snd e)
  where (e:elements) = sortBy (\(a,_) (b,_) -> compare a b) $ map (\y -> (manhattan x y, y)) s

manhattan :: (Int, Int) -> (Int,Int) -> Int
manhattan (x,y) (a,b) = (abs $ x-a) + (abs $ y-b)

star2 :: [(Int, Int)] -> Int
star2 s = foldr (\y -> if sum (map (manhattan y) s) <10000 then (+1) else id) 0 (inbounds s)

inbounds :: [(Int,Int)] -> [(Int,Int)]
inbounds s = [(a,b) | a <- [(down fst s)..(up snd s)], b <- [(down snd s)..(up snd s)]]
  where get a b p = a $ map b p
        up = get maximum
        down = get minimum

main :: IO()
main = do
  text <- readFile "input"
  let coord = Prelude.map (\x -> read $ '(':x++")") $ lines text :: [(Int,Int)]
  putStrLn $ show $ star1 coord
  putStrLn $ show $ star2 coord
