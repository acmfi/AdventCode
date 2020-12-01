main :: IO ()
main = do
  input <- (fmap read) . lines <$> getContents :: IO [Int]
  let star1 = head $ [ x * y | x <- input, y <- input, x + y == 2020]

  putStrLn $ "Star 1: " ++ show star1

  let star2 = head $ [ x * y * z| x <- input, y <- input, z <- input, x + y + z == 2020]
  putStrLn $ "Star 2: " ++ show star2
