
module Main where
import Graphics.Gloss.Interface.Pure.Simulate
import Text.ParserCombinators.ReadP
import Data.Function ((&))
import Graphics.Gloss

type Pos = (Int,Int)
data Star = Star
    { pos :: Pos
    , vel :: Pos
    } deriving Show

data World = World
    { stars :: [Star]
    , time  :: Float
    } deriving Show


isDigit :: Char -> Bool
isDigit c = elem c ['0'..'9']

parse :: ReadP Star
parse = do
    let isNum x = isDigit x || x == '-'
    _ <- munch $ not.isNum
    [x,y,vx,vy] <- readInts
    return $ Star (x,y) (vx,vy)

runP  :: ReadP a -> String -> a
runP parser str = readP_to_S parser str & last & fst
    
readInts :: ReadP [Int]
readInts = do
    let isNum x = isDigit x || x == '-'
        int     = read <$> munch1 isNum
        ignore  = munch $ not.isNum

    int `endBy` ignore

evolve :: [Star] -> [Star]
evolve = map (\star -> Star (new_pos (pos star) (vel star)) (vel star))
  where new_pos = (\(a,b) (c,d) -> (a+c,b+d))

draw_world :: World -> Picture
draw_world w = draw_all $ map pos $ stars w

draw_all :: [Pos] -> Picture
draw_all positions =
  let   (windowWidth, windowHeight) = (maximum $ map fst positions, maximum $ map snd positions)

        offsetX = - fromIntegral windowWidth  / 2
        offsetY = - fromIntegral windowHeight / 2
    in Translate offsetX offsetY
       $ Pictures
       $ map draw_position positions

draw_position :: Pos -> Picture
draw_position (posX, posY) = Color (makeColor 0 0 0 1) (locurote 1 posX posY)

locurote :: Int -> Int -> Int -> Picture
locurote cellSize posXi posYi
 = let  cs      = fromIntegral cellSize
        posX    = fromIntegral posXi
        posY    = fromIntegral posYi
        x1      = posX
        x2      = posX + cs
        y1      = posY
        y2      = posY + cs
   in   Polygon [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]

end :: [Star] -> ([Star], Int)
end s = if size s > size ns then (\(a,b) -> (a, b+1)) $ end ns else (s,0)
  where ns = evolve s

size :: [Star] -> Int
size s = (maximum $ p fst) - (minimum $ p fst) + ((maximum $ p snd) - (minimum $ p snd))
  where p f = map (f . pos) s

main :: IO()
main = do
  -- Run parser and extract all data
  txt <- readFile "input"
  let st = (map (runP parse) $ lines txt)
  let myWorld = World st 0
  --putStrLn $ show myWorld
  -- | star 1

  display (InWindow "Stars" (50,50) (10, 10)) white (draw_all $ map pos $ fst $ end $ stars myWorld)
  simulate (InWindow "Stars" (50,50) (10, 10)) white 10 myWorld draw_world simulateWorld
  -- | Star 2
  putStrLn $ show $ snd $ end $ st

simulateWorld :: ViewPort -> Float -> World -> World
simulateWorld _ time' world
        -- If enough time has passed then it's time to step the world.
        | time world >= 0.01
        = let next_stars = evolve $ stars world
          in  World next_stars 0
        -- Wait some more.
        | otherwise
        = world { time = time world + time' }
