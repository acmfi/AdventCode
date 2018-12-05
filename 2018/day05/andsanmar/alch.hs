import Data.Char (toUpper)

red :: (String, String) -> String
red (q,[]) = q
red ([],(x:xs)) = red ([x],xs)
red (y,(x:xs))
  | react (last y) x = red (init y, xs)
  | otherwise = red (y ++ [x], xs)

same :: Char -> Char -> Bool
same x y = toUpper y == toUpper x

react :: Char -> Char -> Bool
react x y = toUpper y == toUpper x && y /= x

star2 :: String -> Int
star2 s = minimum $ map (\x -> length $ red ([], del x s)) ['a'..'z']

del :: Char -> String -> String
del _ [] = []
del c (x:xs)
  | same c x = del c xs
  | otherwise = x:(del c xs)

main :: IO()
main = do
  text <- readFile "input"
  let reduced = red ([], text)
  putStrLn $ show $ length $ reduced
  putStrLn $ show $ star2 $ reduced
