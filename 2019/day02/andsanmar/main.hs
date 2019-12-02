import System.Environment

star1 :: [Int] -> Int -> Int -- List, Index
star1 l index
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
star2 _ (_,_) = undefined

replace :: [Int] -> Int -> Int -> [Int] -- List, Index, NewValue
replace s n p = (\(l,_:r) -> l ++ n:r) $ splitAt p s

preproc :: [Int] -> (Int, Int) -> (Int, Int) -> [Int]
preproc l (a1,a2) (b1,b2) = replace (replace l a1 a2) b1 b2

main :: IO()
main = do
  (f:_) <- getArgs
  text <-  readFile f
  let nums = read $ ('[':text) ++ "]"
  let r1 = star1 (preproc nums (12,1) (2,2)) 0
  print r1
  let r2 = star2 nums (0,0)
  print r2
