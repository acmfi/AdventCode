-- 1-3 a: abcde
-- 1-3 b: cdefg
-- 2-9 c: ccccccccc
parse :: String -> ((Int, Int), Char, String)
parse input =
  let
    [range, char', passwd] = words input
    char = head char'
    [lower, upper] = read <$> splitOn '-' range :: [Int]
  in
  ((lower, upper), char, passwd)

splitOn :: Char -> String -> [String]
splitOn = splitOn' []
  where
    splitOn' :: String -> Char -> String -> [String]
    splitOn' acc e [] = [reverse acc]
    splitOn' acc e (x:xs)
      | e == x = (reverse acc) : splitOn' [] e xs
      | otherwise = splitOn' (x:acc) e xs

count :: Eq a => a -> [a] -> Int
count e = length . filter (== e)

countBetween :: Eq a => ((Int, Int), a, [a]) -> Bool
countBetween ((lower, upper), e, xs) =
  let
    c = count e xs
  in
    c >= lower && c <= upper

star1 :: [((Int, Int), Char, String)] -> Int
star1 = length . filter countBetween


--

exactAt :: Eq a => ((Int, Int), a, [a]) -> Bool
exactAt ((lower, upper), e, xs) = let
  pos1 = xs !! (lower - 1) == e
  pos2 = xs !! (upper - 1) == e
  in
   if pos1 then not pos2 else pos2


star2 :: [((Int, Int), Char, String)] -> Int
star2 = length . filter exactAt
--

main :: IO ()
main = do
  input <- (fmap parse) . lines <$> getContents
  putStrLn $ "Star 1: " ++ show (star1 input)
  putStrLn $ "Star 2: " ++ show (star2 input)
