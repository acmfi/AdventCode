import System.Environment
import Data.List.Split
import Data.Set

type Path = Set (Int, (Int, Int)) -- Steps & Coord
data Dir = U | D | L | R deriving Show
type Steps = (Int, Dir)

get_dir :: String -> Steps
get_dir ('U':n) = (read n, U)
get_dir ('R':n) = (read n, R)
get_dir ('D':n) = (read n, D)
get_dir ('L':n) = (read n, L)

string_to_path :: String -> [Steps]
string_to_path s = Prelude.foldr (\e l -> (get_dir e):l) [] (splitOn "," s)

inc :: Steps -> (Int, Int) -> (Int, Int)
inc (_, U) (a,b) = (a,b+1)
inc (_, R) (a,b) = (a+1,b)
inc (_, D) (a,b) = (a,b-1)
inc (_, L) (a,b) = (a-1,b)

get_path :: [Steps] -> Int -> (Int, Int) -> Path
get_path (x:xs) steps (a, b) = insert (steps, (a, b)) $ get_path (new_ls x) (steps+1) (inc x (a, b))
  where new_ls (1, _) = xs
        new_ls (n, d) = (n-1, d):xs
get_path [] steps (a, b) = singleton (steps, (a, b))

coord_to_manh (a,b) = (abs a + abs b, (a,b))

main :: IO()
main = do
  (f:_) <- getArgs
  text <-  readFile f
  let [dirs1, dirs2] = Prelude.map string_to_path (lines text)
  let dir1proc = delete (0, (0,0)) $ get_path dirs1 0 (0,0)
  let dir2proc = delete (0, (0,0)) $ get_path dirs2 0 (0,0)
  let intersection_points = intersection (Data.Set.map snd dir1proc) (Data.Set.map snd dir2proc)
  let r1 = Data.Set.map coord_to_manh intersection_points
  print r1
  let dir1_intersection_points = Data.Set.filter (\el -> member (snd el) intersection_points) dir1proc
  let dir2_intersection_points = Data.Set.filter (\el -> member (snd el) intersection_points) dir2proc
  let coinc = [(x, y) | x <- (toList dir1_intersection_points), y <- (toList dir2_intersection_points), snd x == snd y]
  print $ minimum $ Prelude.map (\((a,_),(b,_)) -> a+b) coinc
