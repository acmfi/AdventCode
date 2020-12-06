#!/usr/bin/env stack
-- stack --resolver lts-16.24 script --package containers

data Forest = Tree | Open deriving (Show, Eq)

type Slope = (Int, Int)

toboggan :: Slope -> [String] -> String
toboggan _               []       = ""
toboggan slope@(_, down) (l : ls) = go slope down width 1 (l : ls)
 where
  width = length l

  go
    :: Slope    -- Slope taken
    -> Int      -- Current slope down index. Used for skip a line
    -> Int      -- Width of the board
    -> Int      -- Current down position
    -> [String] -- List of inputs
    -> String   -- Result
  go _ _ _ _ [] = mempty
  go (r, d) 0 w i (x : xs) = --
    (x !! ((r * i) `mod` w)) : go (r, d) d w (i + 1) (x : xs)
  go (r, d) n w i (_ : xs) = go (r, d) (n - 1) w i xs -- We skip this line of trees

star1 :: [String] -> Int
star1 = length . filter (== '#') . toboggan (3, 1)

star2 :: [String] -> Int
star2 input = product $ map (\slope -> length . filter (== '#') $ toboggan slope input) slopes
 where
  slopes :: [Slope]
  slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main :: IO ()
main = do
  input <- lines <$> getContents

  putStrLn $ "Star 1: " ++ show (star1 input)
  putStrLn $ "Star 2: " ++ show (star2 input)
