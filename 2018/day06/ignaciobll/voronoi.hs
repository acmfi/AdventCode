{-# LANGUAGE FlexibleContexts #-}
import Data.Maybe
import qualified Data.Set as Set
import Data.List
import Text.Parsec
import Text.Parsec.Char
import Control.Applicative hiding (many, (<|>))
import Data.Either (rights)

type Point = (Int, Int)
type Class = Point

pPoint :: Stream s m Char => ParsecT s u m Point
pPoint = do
  a <- rd <$> many digit
  string ", "
  b <- rd <$> many digit
  return $ (a,b)
  where rd = read :: String -> Int

manhattan :: Point -> Point -> Int
manhattan (a,b) (c,d) = (abs $ a - c) + (abs $ b - d)


choose :: [Class] -> Point -> Maybe Class
choose  cs p = case maybeMin (map (manhattan p) cs) of
                 Nothing -> Nothing
                 Just _ -> Just $ choose' cs p

choose' :: [Class] -> Point -> Class
choose' cs p = snd . foldr1 min $ map (\c -> (manhattan p c, c)) cs

maybeMin :: Ord a => [a] -> Maybe a
maybeMin []     = Nothing
maybeMin (x:xs) = case foldl min' (x, False) xs of
                    (v, False) -> Just v
                    _ -> Nothing

min' :: Ord a => (a, Bool) -> a -> (a, Bool)
min' (x, r) y
  | x == y    = (x, True)
  | y < x     = (y, False)
  | otherwise = (x, r)

bounds :: [Class] -> (Point, Point)
bounds cs = ((tl, tl'), (br,br'))
  where
    tl  = foldr1 min $ map fst cs
    tl' = foldr1 min $ map snd cs
    br  = foldr1 max $ map fst cs
    br' = foldr1 max $ map snd cs

range :: (Point, Point) -> [Point]
range ((x,y), (x',y')) = [(a,b) | a <- [x..x'], b <- [y..y']]

perimeter :: (Point, Point) -> [Point]
perimeter ((a, b), (c, d)) = Set.toList $ Set.fromList (topbottom ++ leftright)
  where
    topbottom  = [a..c] >>= (\x -> [(x,b), (x,d)])
    leftright  = [b..d] >>= (\x -> [(a,x), (c,x)])

perimeterClasses :: [Class] -> [Point] -> [Class]
perimeterClasses cs ps = catMaybes $ map (choose cs) ps


star1 :: [Class] -> Int
star1 cs = foldr1 max qtty
  where qtty = (map length $ group $ sort $ foldr (\h a -> [x | x <- a, x /= h]) ps validClasses)
        ps = catMaybes $ map (choose cs) (range $ bounds cs)
        validClasses = perimeterClasses cs . perimeter $ bounds cs

accDistanceLEThan :: Int -> [Class] -> Point -> Bool
accDistanceLEThan n cs p = n >= accDistance cs p

accDistance :: [Class] -> Point -> Int
accDistance cs p = sum $ map (manhattan p) cs

star2 :: [Class] -> Int
star2 cs = length $ filter id $ map (accDistanceLEThan 10000 cs) (range $ bounds cs)

main :: IO ()
main = do
  contents <- getContents
  let cs = rights $ fmap (parse pPoint "") $ lines contents
  putStrLn $ "Star 1: " ++ (show $ star1 cs)
  putStrLn $ "Star 2: " ++ (show $ star2 cs)
