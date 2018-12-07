import Data.Map.Strict as Map (fromListWith, delete, elems, Map)
import Data.List (sort)
import Data.Char (ord)

star1 :: Map Char String -> String
star1 s = next s ""

next :: Map Char String -> String -> String
next dependencies current = if sort current == ['A'..'Z'] then current else next (delete (head can_take) dependencies) (current++[head can_take])
  where can_take = sort $ filter (\x -> not $ elem x current) $ notConf dependencies

notConf :: Map Char String -> String
notConf m = filter (\x -> not $ any (==True) $ map (elem x) (elems m)) ['A'..'Z']

main :: IO()
main = do
  text <- readFile "input"
  let dat = fromListWith (++) $ map (\x -> (head $ words x !! 1, words x !! 7)) $ lines text :: Map Char String
  putStrLn $ show $ star1 dat
