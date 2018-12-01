import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

star1 :: Integral a => [a] -> a
star1 = sum


-- 0,03s user 0,01s system 98% cpu 0,035 total
star2'' :: [Int] -> Int
star2'' ls = go 0 (IntSet.singleton 0) rep
  where rep = concat $ repeat ls
        go freq set (x:xs)
          | IntSet.member (freq+x) set = freq+x
          | otherwise = go (freq + x) (IntSet.insert (freq + x) set) xs


-- 0,17s user 0,01s system 99% cpu 0,176 total
star2' :: [Int] -> Int
star2' ls = go 0 (Set.singleton 0) rep
  where rep = concat $ repeat ls
        go freq set (x:xs)
          | Set.member (freq+x) set = freq+x
          | otherwise = go (freq + x) (Set.insert (freq + x) set) xs

-- 123,00s user 0,01s system 99% cpu 2:03,58 total
star2 :: Integral a => [a] -> a
star2 ls = go 0 [0] rep
  where rep = concat $ repeat ls
        go freq set (x:xs)
          | elem (freq+x) set = freq+x
          | otherwise = go (freq + x) ((freq + x):set) xs

format :: [Char] -> [Char]
format ('+':num) = num
format num = num

main :: IO ()
main = do
  contents <- getContents
  let numbers = (map (read . format) $ lines contents) :: [Int]
  putStrLn $ "Star 1: " ++ (show $ star1 numbers)
  putStrLn $ "Star 2: " ++ (show $ star2'' numbers)
