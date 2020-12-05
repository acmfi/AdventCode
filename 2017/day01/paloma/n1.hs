import Data.Char

-- Part 1
captcha :: String -> Int
captcha input = sum (matching2 (parInput) 2)
  where parInput = map digitToInt input

-- Part 2  
captcha2 :: String -> Int
captcha2 input = sum (matching2 (parInput) (distance parInput))
  where parInput = map digitToInt input

distance :: [Int] -> Int
distance list = (length list) `div` 2

matching2 :: [Int] -> Int -> [Int]
matching2 input dist = map (\(e, n) -> (if ((input !! n) == (input !! ((n + dist) `mod` (length input)))) then e else 0 )) (zip input [0..])
