import Data.Map.Strict as Map (Map, lookup, fromList)
import Data.Maybe (fromJust)
--import Data.List (minimumBy, delete)
import Text.ParserCombinators.ReadP
import Data.Function ((&))

type Pos = (Int,Int)
data Dir = N | S | E | W deriving Show
data Track = Turn1 | Turn2 | Inter deriving Show
data Intersection = L | St | R deriving Show
data Cart = Cart
  { pos :: Pos
  , dir :: Dir
  , inter :: Intersection
  } deriving Show

parseCircuit :: ReadP ((Map Pos Track), [Cart])
parseCircuit = do
  let isC x = elem x "^><v"
      isT x = elem x "\\/+"
  text <- look
  let (a,b,_) = foldl (\(x,y,z) c -> if isC c then (x, (Cart z (givDir c) L):y, nextP c z) else if isT c then ((z,givTrack c):x,y, nextP c z) else (x,y, nextP c z)) ([],[],(0,0)) text
  return $ (fromList a, b)
  where givDir '>' = E
        givDir '^' = N
        givDir '<' = W
        givDir 'v' = S
        givDir _ = undefined
        givTrack '/' = Turn2
        givTrack '\\' = Turn1
        givTrack '+' = Inter
        givTrack _ = undefined
        nextP c = if c == '\n' then (\(x,_) -> (x+1,0)) else (\(x,y) -> (x,y+1))

runP :: ReadP a -> String -> a
runP parser str = readP_to_S parser str & last & fst


nI :: Intersection -> Intersection
nI L = St
nI St = R
nI R = L

doInter :: Dir -> Intersection -> Dir
doInter x St = x
doInter x L = case x of N -> W
                        S -> E
                        E -> N
                        W -> S
doInter x R = case x of N -> E
                        S -> W
                        E -> S
                        W -> N

nd :: Dir -> Track -> Dir
nd x Turn1 = case x of N -> W
                       S -> E
                       E -> S
                       W -> N
nd x Turn2 = case x of N -> E
                       S -> W
                       E -> N
                       W -> S
nd _ _ = undefined

next :: Map Pos Track -> Cart -> Cart
next m c = case to_do of Nothing -> nP
                         Just Inter -> (\x -> Cart (pos x) dirAfterI (nI $ inter x)) $ go c dirAfterI
                         _ -> go c (nd (dir c) (fromJust to_do))
  where to_do = Map.lookup (pos c) m
        nP = go c (dir c)
        dirAfterI = doInter (dir c) (inter c)

-- route :: Map Pos Track -> Cart -> [Pos]
-- route m c = (pos c):(route m $ next m c)

go :: Cart -> Dir -> Cart
go c N = Cart ((\(a,b) -> (a-1,b)) $ pos c) N (inter c)
go c S = Cart ((\(a,b) -> (a+1,b)) $ pos c) S (inter c)
go c E = Cart ((\(a,b) -> (a,b+1)) $ pos c) E (inter c)
go c W = Cart ((\(a,b) -> (a,b-1)) $ pos c) W (inter c)

firstCollision :: Map Pos Track -> [Cart] -> Pos
firstCollision c s = if length t == 0
  then firstCollision c $ map (next c) s
  else pos $ head t
  where t = [x | x <- s, (length $ filter (\y -> pos y == pos x) s) >= 2]

lastRemaining :: Map Pos Track -> [Cart] -> Cart
lastRemaining circ s = if length remaining == 1 then head remaining
                       else lastRemaining circ $ map (next circ) remaining
  where remaining = deleteIfDup (\x y -> pos x /= pos y) s

deleteIfDup :: (Cart -> Cart -> Bool) -> [Cart] -> [Cart]
deleteIfDup _ [] = []
deleteIfDup f (x:xs) = if (pos x) `elem` (map pos xs) then deleteIfDup f $ filter (f x) xs else x:(deleteIfDup f xs)

main :: IO ()
main = do
  text <- readFile "input"
  let (circ,cars) = runP parseCircuit text
  putStrLn $ show $ (\(a,b) -> (b,a)) $ firstCollision circ cars
  putStrLn $ show $ (\(a,b) -> (b,a)) $ pos $ lastRemaining circ cars
  return ()
