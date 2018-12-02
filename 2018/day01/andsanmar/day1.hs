import qualified Data.IntSet as IntSet

star2 :: [Int] -> Int
star2 x = fset (concat $ repeat x) (IntSet.empty) 0

fset :: [Int] -> IntSet.IntSet -> Int -> Int
fset (l:ls) xs s = if IntSet.member s xs then s else fset ls (IntSet.insert s xs) cur_sum
  where cur_sum = s + l
fset [] _ _ = 0

read' :: String -> Int
read' ('+':x) = read x
read' x = read x

main :: IO()
main = do
  text <-  readFile "input"
  let nums = Prelude.map read' $ lines text
  putStrLn $ show $ sum nums
  putStrLn $ show $ star2 nums
