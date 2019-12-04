list_digits l = map (\x -> read [x] :: Int) (show l)
ordered l = all (uncurry (<=)) $ zip (init l) (tail l)
reps xs  = [length $ filter (==x) xs | x <- [0..9]]
meets x = ordered l && any (>=2) (reps l)
  where l = list_digits x
        
main :: IO ()
main = do
  let list = [372037..905157]
  let r1 = [x | x <- list, meets x]
  print $ length r1
  print $ length [x | x <- r1, 2 `elem` (reps $ list_digits x)]
  
