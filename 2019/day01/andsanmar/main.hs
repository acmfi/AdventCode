import System.Environment

transf :: Integer -> Integer
transf x = (div x 3) - 2

requireFuel :: Integer -> [Integer]
requireFuel x = if x <= 0 || newfuel <= 0 then [0] else newfuel:(requireFuel newfuel)
  where newfuel = transf x

main = do
  (f:_) <- getArgs
  s  <- readFile f
  let result = map (\x -> transf $ read x) (lines s)
  -- putStrLn $ show (result)
  putStrLn $ show (sum result)
  let result2 = map (\r -> requireFuel r) result
  -- putStrLn $ show result2
  putStrLn $ show (sum result + sum (map sum result2))
