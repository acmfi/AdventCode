input :: Int
input = 289326

type Coord = (Int, Int)

coordPred :: Coord -> [Coord]
coordPred (0, 0) = []
coordPred (1, 0) = [(0,0)]
coordPred (x,y)
  | x == y && pos x = [(x, y-1), (x-1, y-1)]
  | x == y && neg x = [(x, y+1), (x+1, y+1)]
  | -x == y && neg x = [(x+1, y), (x+1, y-1)]
  | -x == y && pos x = [(x, y+1), (x-1, y+1), (x-1, y)]
  | abs x < abs y && pos y = [(x+1, y), (x+1, y-1), (x, y-1)]
  | abs x < abs y && neg y = [(x, y+1), (x-1, y), (x-1, y+1)]
  | abs y < abs x && pos x = [(x, y-1), (x-1, y-1), (x-1, y)]
  | abs y < abs x && neg x = [(x, y+1), (x+1, y), (x+1, y+1)]
  
neg :: Int -> Bool
neg n
  | n < 0 = True
  | otherwise = False

pos :: Int -> Bool
pos = not . neg

intToCoord' :: Int -> Coord -> Coord
intToCoord' 1 _ = (0,0)
intToCoord' 2 _ = (1,0)
intToCoord' _ (0, y')
  | neg y' = (1, y')
  | otherwise = (-1, y')
intToCoord' _ (x', 0)
  | neg x' = (x', -1)
  | otherwise = (x', 1)
intToCoord' _ (x', y')
  | x' == y' && neg x' = (x' + 1, y')
  | -x' == y' && neg y' = (x' + 1, y')
  | -x' == y' && neg x' = (x', y' - 1)
  | x' == y' = (x' - 1, y')
  | abs x' < abs y' && pos y' = (x' - 1, y')
  | abs x' < abs y' && neg y' = (x' + 1, y')
  | abs y' < abs x' && pos x' = (x', y' + 1)
  | abs y' < abs x' && neg x' = (x', y' - 1)

intToCoord :: Int -> Coord
intToCoord 1 = (0,0)
intToCoord 2 = (1,0)
intToCoord n = intToCoord' n (intToCoord (n - 1))

manhattan :: Coord -> Int
manhattan (x,y) = abs x + abs y

val' :: Coord -> Int
val' (0,0) = 1
val' (1,0) = 1
val' coord = sum (map val' (coordPred coord))

val :: Int -> Int
val = val' . intToCoord

sol :: Int -> Int
sol n 
  | val n > input = n
  | otherwise = sol (n + 1)

main :: IO ()
main = print $ sol 1
