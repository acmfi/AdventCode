list_digits l = map (\x -> (read [x] :: Int)) (show l)
with_next l = zip (init l) (tail l)
adj_digits l = any (\(a,b) -> a == b) l
ordered l = all (\(a,b) -> a <= b) l

only_twice xs = any (==2) [length $ filter (==x) xs | x <- [0..9]]

meets :: Int -> Bool
meets x = ordered l && adj_digits l
  where l = with_next $ list_digits x

meets2 x = ordered l && adj_digits l && only_twice (list_digits x)
  where l = with_next $ list_digits x
        
main :: IO ()
main = do
  let list = [372037..905157]
  print $ length [x | x <- list, meets x]
  print $ length [x | x <- list, meets2 x]
  
