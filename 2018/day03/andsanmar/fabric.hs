import Data.Set

type Claim = (Int,(Int,Int),(Int,Int))

parse_claim :: String -> Claim
parse_claim s = (id_num,pos,dimension)
  where id_num = read $ tail $ takeWhile (/='@') s
        im = tail $ dropWhile (/='@') s
        pos = read $ '(':(takeWhile (/=':') im) ++ ")"
        st_dim = tail $ dropWhile (/=':') im
        dimension = (read (takeWhile (/='x') st_dim), read (tail$ dropWhile (/='x') st_dim))

star1 :: [Claim] -> Int
star1 c = length $ toList $ fst $ Prelude.foldl (\(a,b) s -> (union a (intersection b s), union b s)) (empty,empty) l
          where l = Prelude.map claim_to_set c

claim_to_set :: Claim -> Set (Int,Int)
claim_to_set (_,(x1,y1),(a1,b1)) = fromList $ concat $ Prelude.foldl (\l v -> (zip [x1..x1+a1-1] (repeat v)):l) [] [y1..y1+b1-1]

star2 :: [Claim] -> Int
star2 (g@(id_num,_,_):cs) = if all (\x -> Data.Set.null (intersection c x)) (Prelude.map claim_to_set cs) then id_num else star2 (cs++[g])
  where c = claim_to_set g
star2 _ = undefined

main :: IO()
main = do
  text <-  readFile "input"
  let claims = Prelude.map parse_claim $ lines text
  putStrLn $ show $ star1 $ claims
  putStrLn $ show $ star2 $ claims
