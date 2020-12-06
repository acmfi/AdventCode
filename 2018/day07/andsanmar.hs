import Data.Map.Strict as Map (fromListWith, delete, elems, Map)
import Data.List (sort)
import Data.Char (ord)

star1 :: Map Char String -> String
star1 s = next s ""

star2 :: Map Char String -> Int
star2 dependencies = advance (dependencies, "", replicate 5 (Nothing, 0))

advance :: (Map Char String, String, [(Maybe Char, Int)]) -> Int
advance (z,a,b) = 1 + if sort a == ['A'..'Z'] && (replicate 5 Nothing) == map fst b then 1 else advance $ foldl step (z,a,[]) b

step :: (Map Char String, String, [(Maybe Char, Int)]) -> (Maybe Char, Int) -> (Map Char String, String, [(Maybe Char, Int)])
step (dic,current,l) (a,b)
  | b <= 0 && a /= Nothing && news /= [] = (dic_el, (head news):current, (Just (head news),ord (head news) - 4):l)
  | b <= 0 && a /= Nothing && news == [] = (dic_el, current, (Nothing,0):l)
  | a == Nothing && can_take /= [] = (dic_c, (head can_take):current, (Just (head can_take),ord (head can_take) - 4):l)
  | otherwise = (dic, current, (a,b-1):l)
  where Just u = a
        dic_el = delete u dic
        news = sort $ filter (\x -> not $ elem x current) $ notConf dic_el
        can_take = sort $ filter (\x -> not $ elem x current) $ notConf dic
        dic_c = delete (head can_take) dic

next :: Map Char String -> String -> String
next dependencies current = if sort current == ['A'..'Z'] then current else next (delete (head can_take) dependencies) (current++[head can_take])
  where can_take = sort $ filter (\x -> not $ elem x current) $ notConf dependencies

notConf :: Map Char String -> String
notConf m = filter (\x -> not $ any (==True) $ map (elem x) (elems m)) ['A'..'Z']

main :: IO()
main = do
  text <- readFile "../skgsergio/input"
  let dat = fromListWith (++) $ map (\x -> (head $ words x !! 1, words x !! 7)) $ lines text :: Map Char String
  putStrLn $ show $ star1 dat
  putStrLn $ show $ star2 dat
