data Rot = R | L deriving (Read, Show)
data Dir = N | S | E | W deriving (Read, Show)

star1 :: [(Rot,Int)] -> Int
star1 s = distance $ foldl (\y r -> head $ advance y r) (N,0,0) s

distance :: (Num a) => (b,a,a) -> a
distance (_,a,b) = abs a + abs b

advance :: (Dir,Int,Int) -> (Rot,Int) -> [(Dir,Int,Int)]
advance (dir,a,b) (rot,x) = case dir of N -> case rot of L -> [(W,y,b) | y <- [a-x..a]]
                                                         R -> reverse [(E,y,b) | y <- [a..a+x]]
                                        S -> case rot of L -> reverse [(E,y,b) | y <- [a..a+x]]
                                                         R -> [(W,y,b) | y <- [a-x..a]]
                                        E -> case rot of L -> reverse [(N,a,y) | y <- [b..b+x]]
                                                         R -> [(S,a,y) | y <- [b-x..b]]
                                        W -> case rot of L -> [(S,a,y) | y <- [b-x..b]]
                                                         R -> reverse [(N,a,y) | y <- [b..b+x]]

star2 :: [(Rot,Int)] -> Int
star2 s = abs p + abs q
  where (p,q) = fst_repetition . reverse $ map (\(_,a,b) -> (a,b)) $ foldl (\(x:xs) r -> (advance x r) ++ xs) [(N,0,0)] s

fst_repetition :: (Eq a) => [a] -> a
fst_repetition [] = error "Empty list"
fst_repetition (x:xs) = if elem x xs then x else fst_repetition xs

main :: IO ()
main = do
  text <- readFile "input1"
  let dat = map (\(x:xs) -> read $ '(':x:',':takeWhile (/=',') xs ++ ")") (words text) :: [(Rot,Int)]
  putStrLn $ show $ star1 dat
  putStrLn $ show $ star2 dat
