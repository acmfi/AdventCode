data Dir = N | NE | SE | S | SW | NW deriving Show

split' :: Eq a => a -> [a] -> [[a]]
split' _ [] = []
split' d s = x : split' d (drop 1 y) where (x,y) = span (/= d) s

split :: String -> [String]
split = split' ','

strToDir :: String -> Maybe Dir
strToDir "n" = Just N
strToDir "ne" = Just NE
strToDir "se" = Just SE
strToDir "s" = Just S
strToDir "sw" = Just SW
strToDir "nw" = Just NW
strToDir _ = Nothing

move :: Dir -> (Float, Float) -> (Float, Float)
move N (x,y) = (x-1, y+1)
move NE (x,y) = (x,y+1)
move SE (x,y) = (x+1,y)
move S (x,y) = (x+1,y-1)
move SW (x,y) = (x,y-1)
move NW (x,y) = (x-1,y)

joinDirs :: [Maybe Dir] -> Maybe [Dir]
joinDirs [] = Just []
joinDirs [Just d] = Just [d]
joinDirs [Nothing] = Nothing
joinDirs (Just d:ds) = case joinDirs ds of
  (Just dss) -> Just (d:dss)
  Nothing -> Nothing
joinDirs (Nothing:_) = Nothing
  
distance :: (Float,Float) -> (Float,Float) -> Float
distance (x,y) (x',y') = let dx = abs (x' - x)
                             dy = abs (y' - y)
                             z1 = -(x + y)
                             z2 = -(x' + y')
                             dz = abs (z2 - z1)
                         in maximum [dx, dy, dz]

traveledPositions :: (Float,Float) -> [Dir] -> [(Float,Float)]
traveledPositions _ [] = []
traveledPositions v (d:ds) = let next = move d v in next:traveledPositions next ds

solve :: String -> Maybe Float
solve s = let initial = (0,0)
          in joinDirs (map strToDir (split s)) >>=
             Just . traveledPositions initial >>=
             Just . maximum . map (distance initial)

removeNl :: String -> String
removeNl [] = []
removeNl ('\n':s) = removeNl s
removeNl (s:ss) = s:removeNl ss

main :: IO ()
main = do
  input <- fmap removeNl (readFile "input")
  case solve input of
    Just n -> print n
    Nothing -> putStrLn "No solution"
