import Data.Tree

star1 :: [Int] -> (Int,[Int])
star1 (0:m:s) = (sum $ take m s, drop m s)
star1 (n:m:s) = (\(a,b) -> (a+(sum $ take m b), drop m b)) $ foldl (\(l,r) _ -> (l + (fst $ star1 r), snd $ star1 r)) (0,s) [1..n]

star2 :: [Int] -> (Int,[Int])
star2 (0:m:s) = (sum $ take m s, drop m s)
star2 (n:m:s) = (\(a,b) -> (foldl (\l y -> (l + if y > n then 0 else a !! (y-1))) 0 (take m b), (drop m b))) $ foldl (\(l,r) _ -> (l ++ [fst $ star2 r], snd $ star2 r)) ([],s) [1..n]

main :: IO()
main = do
  text <- readFile "input"
  let dat = map read $ words text :: [Int]
  putStrLn $ show $ star1 dat
  putStrLn $ show $ star2 dat
