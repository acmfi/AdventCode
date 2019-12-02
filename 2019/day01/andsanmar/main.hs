import System.Environment

transf :: Integer -> Integer
transf x = div x 3 - 2

requireFuel :: Integer -> [Integer]
requireFuel x = if x <= 0 || newfuel <= 0 then [0] else newfuel:requireFuel newfuel
  where newfuel = transf x

main :: IO ()
main = do
  (f:_) <- getArgs
  s  <- readFile f
  let result = map (transf . read) (lines s)
  -- putStrLn $ show (result)
  print (sum result)
  let result2 = map requireFuel result
  -- putStrLn $ show result2
  print (sum result + sum (map sum result2))
