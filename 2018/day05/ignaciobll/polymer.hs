import Data.Char

-- Usado para resolver la primera estrella.
-- Generalizado para la segunda.

-- react :: String -> Int
-- react = length . (react' [])

-- react' :: String -> String -> String
-- react'  x []     = x
-- react' [] (x:xs) = react' [x] xs
-- react' (x:xs) (y:ys)
--   | cond x y  = react' xs ys
--   | otherwise = react' (y:x:xs) ys

react :: String -> Int
react = reactWith (const False)

reactWith ::(Char -> Bool) -> String -> Int
reactWith f = length . (react'' f [])

cond :: Char -> Char -> Bool
cond a b = (isUpper a && isLower b || isLower a && isUpper b) &&
           (toLower a == toLower b)

react'' :: (Char -> Bool) -> String -> String -> String
react'' _ x [] = x
react'' f [] (x:xs)
  | f x = react'' f [] xs
  | otherwise = react'' f [x] xs
react'' f (x:xs) (y:ys)
  | f y = react'' f (x:xs) ys
  | cond x y  = react'' f xs ys
  | otherwise = react'' f (y:x:xs) ys

star2 :: String -> Int
star2 s = foldr min maxBound polymers
  where polymers = zipWith reactWith conditions (repeat s)
        conditions = map (\c -> \c' -> c == (toLower c')) ['a'..'z']

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ "Star 1: " ++ (show $ react (init contents))
  putStrLn $ "Star 2: " ++ (show $ star2 (init contents))
