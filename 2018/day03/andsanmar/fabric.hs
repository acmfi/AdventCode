import Data.Set

type Claim = ((Int,Int),(Int,Int))

parse_claim :: String -> Claim
parse_claim s = (pos,dimension)
  where im = tail $ dropWhile (/='@') s
        pos = read $ '(':(takeWhile (/=':') im) ++ ")"
        st_dim = tail $ dropWhile (/=':') im
        dimension = (read (takeWhile (/='x') st_dim), read (tail$ dropWhile (/='x') st_dim))

star1 :: [Set (Int,Int)] -> Int
star1 c = length $ toList $ fst $ Prelude.foldl (\(a,b) s -> (union a (intersection b s), union b s)) (empty,empty) c

claim_to_set :: Claim -> Set (Int,Int)
claim_to_set ((x1,y1),(a1,b1)) = fromList $ [(x,y) | x <- [x1..x1+a1-1], y <- [y1..y1+b1-1]]

star2 :: [Set (Int,Int)] -> Int -> Int
star2 (c:cs) n = if all (\x -> Data.Set.null (intersection c x)) cs then n else star2 (cs++[c]) (n+1)
star2 _ _ = undefined

main :: IO()
main = do
  text <-  readFile "input"
  let claims = Prelude.map (claim_to_set . parse_claim) $ lines text
  putStrLn $ show $ star1 claims
  putStrLn $ show $ star2 claims 1
