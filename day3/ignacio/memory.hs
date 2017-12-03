progression 0 = 1
progression 1 = 9
progression n = (n * 2 + 1) * 4 - 4

corner = map progression [0..]

cornerStart n = sum $ take n corner

cornerValue :: [(Int, Int)]
cornerValue = scanl (\(c, v) c' -> (c', v + progression c') ) (0, 0) [1..]

find :: Int -> (Int, Int)
find memAddress = whichCornerValue cornerValue memAddress
    where whichCornerValue ((c, v):xs) memAddress -- no base case bc of infite structure
              | memAddress < v = (c, v)
              | otherwise = whichCornerValue xs memAddress


sideSize n = n * 2 + 1

memAddressToCoor :: Int -> (Int, Int)
memAddressToCoor memAddress = 
    case div offset (side - 1)of
         0 -> if mod offset (side - 1) == 0 then (c - 1, -c + 1) else (c, -c + offset)
         1 -> (c - (offset - side) - 1 , c)
         2 -> (-c, c - (mod offset (side - 1)))
         3 -> (-c, -c + (mod offset (side - 1)))
         otherwise -> (-1,-1)
    where (c, v) = find memAddress
          side = sideSize c
          offset = memAddress - (snd $ cornerValue !! (c - 1)) -- unsafe (negative index)


manhattanDistance (a, b) = abs(a) + abs(b)
