import System.Environment
import Data.List.Split
import Data.Set

type Path = Set (Int, (Int, Int)) -- Steps & Coord
data Dir = U | D | L | R deriving Read
type Steps = (Int, Dir)

getDir :: String -> Steps
getDir (l:n) = (read n, read [l])
getDir _ = undefined

stringToPath :: String -> [Steps]
stringToPath s = Prelude.map getDir (splitOn "," s)

inc :: Steps -> (Int, Int) -> (Int, Int)
inc (_, U) (a,b) = (a,b+1)
inc (_, R) (a,b) = (a+1,b)
inc (_, D) (a,b) = (a,b-1)
inc (_, L) (a,b) = (a-1,b)

getPath :: Int -> (Int, Int) -> [Steps] -> Path
getPath steps (a, b) (x:xs) = insert (steps, new_pos) $ getPath (steps+1) new_pos new_ls
  where new_ls = if fst x == 1 then xs else (fst x -1,snd x):xs
        new_pos = inc x (a, b)
getPath _ _ [] = empty

coordToManh :: (Int, Int) -> Int
coordToManh (a,b) = abs a + abs b

main :: IO()
main = do
  (f:_) <- getArgs
  text <-  readFile f
  let [dirs1, dirs2] = Prelude.map (getPath 1 (0,0) . stringToPath) (lines text)
  let intersection_points = intersection (Data.Set.map snd dirs1) (Data.Set.map snd dirs2)
  print $ head $ toList $ Data.Set.map coordToManh intersection_points
  let dir1_intersection_points = Data.Set.filter (\el -> member (snd el) intersection_points) dirs1
  let dir2_intersection_points = Data.Set.filter (\el -> member (snd el) intersection_points) dirs2
  let coinc = [(x, y) | x <- toList dir1_intersection_points, y <- toList dir2_intersection_points, snd x == snd y]
  print $ minimum $ Prelude.map (\((a,_),(b,_)) -> a+b) coinc
