import System.Environment
import Control.Monad

star1 :: [Int] -> Int -> Int -- List, Index
star1 l index
  | index >= length l = head l -- Limit
  | l !! index /= 1 && l !! index /= 2 = head l -- Opcodes
  | i1 >= length l || i2 >= length l || out >= length l = head l
  | op == 1 = star1 (newList (+)) newIndex
  | op == 2 = star1 (newList (*)) newIndex
  | otherwise = head l
  where (op:i1:i2:out:_) = drop index l
        newIndex = index+4
        newList f = replace l (f (l !! i1) (l !! i2)) out

star2 :: [Int] -> (Int, Int) -> (Int, Int)
star2 l (i1, i2)
  | sol == 19690720 = (i1, i2)
  | sol > 19690720 = star2 l (i1-1, i2)
  | sol < 19690720 = star2 l (i1+1, i2+1)
  where sol = star1 (preproc l (i1, 1) (i2, 2)) 0

replace :: [Int] -> Int -> Int -> [Int] -- List, Index, NewValue
replace l n p = (\(l,(_:r)) -> l ++ n:r) $ splitAt p l

preproc :: [Int] -> (Int, Int) -> (Int, Int) -> [Int]
preproc l (a1,a2) (b1,b2) = (replace (replace l a1 a2) b1 b2)

main :: IO()
main = do
  (f:_) <- getArgs
  text <-  readFile f
  let nums = read $ ('[':text) ++ "]"
  putStrLn $ show nums
  let r1 = star1 (preproc nums (12,1) (2,2)) 0
  putStrLn $ show r1
  let r2 = star2 nums (0,0)
  putStrLn $ show r2
  -- putStrLn $ show $ star2 ids
