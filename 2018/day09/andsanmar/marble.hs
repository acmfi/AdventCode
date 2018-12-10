import Data.Sequence as Seq (Seq((:<|)), (><), (<|), (|>), singleton, splitAt, lookup, length, (!?))
import System.Environment (getArgs)

star1 :: Int -> Int -> Int
star1 p n = (\(_,_,x) -> maximum x) $ foldl insert ((singleton 0)|>1,1,replicate p 0) [2..n]

insert :: (Seq Int, Int, [Int]) -> Int -> (Seq Int, Int, [Int])
insert (marbles,toIns,c) n = if mod n 23 == 0
  then ((\(x,_:<|ys) -> x >< ys) $ Seq.splitAt (toIns-7) marbles, toIns-7,
        (\(x,y:ys) -> x ++ (y + n + u):ys) $ Prelude.splitAt (mod n (Prelude.length c)) c)
  else ((\(x,y) -> x >< n<|y) $ Seq.splitAt ((toIns+2) `mod` (Seq.length marbles)) marbles, (toIns+2) `mod` (Seq.length marbles), c)
  where Just u = marbles !? ((toIns-7) `mod` Seq.length marbles)

main :: IO()
main = do
  args <- getArgs
  let players = read $ args !! 0
  let marbles = read $ args !! 1
  putStrLn $ show $ star1 players marbles
  putStrLn $ show $ star1 players (marbles*100)

