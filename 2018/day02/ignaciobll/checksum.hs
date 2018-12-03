import Data.List (sort, group)
import Control.Monad (replicateM)

star1 :: [String] -> Int
star1 xs = (count 2 xs) * (count 3 xs)
  where count n = (length . filter id . map (rep n))
        rep e xs = elem e (map length $ group $ sort $ xs)

hammingWith :: [String] -> (Int, (String, String))
hammingWith = (\(x:y:[]) -> (hamming x y, (x, y)))
  where hamming a b = length $ filter id $ zipWith (/=) a b

star2 :: [String] -> String
star2 xs = build $ (\(a,b) -> diff a b) $ pair $ filter (\(c,p) -> c == 1) $ map hammingWith $ replicateM 2 xs
  where pair = snd . head
        diff = zipWith (\a b -> (a == b, a))
        build = foldr (\(bool, c) acc -> if bool then c:acc else acc) "" :: [(Bool, Char)] -> String

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ "Star 1: " ++ (show $ star1 $ lines contents)
  putStrLn $ "Star 2: " ++ (show $ star2 $ lines contents)
