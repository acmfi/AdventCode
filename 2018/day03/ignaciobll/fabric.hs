{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
import Text.Parsec.Char
import Control.Applicative hiding (many)
import qualified Data.Map.Strict as Map
import Data.Either (rights)
import Data.Maybe (catMaybes)

data Claim = Claim
  { cid :: Int
  , paddingLeft :: Int
  , paddingTop :: Int
  , width :: Int
  , height :: Int } deriving Show

claim :: Stream s m Char => ParsecT s u m Claim
claim = do
  string "#"
  cid <- integer
  string " @ "
  pL <- integer
  char ','
  pT <- integer
  string ": "
  w <- integer
  string "x"
  h <- integer
  return $ Claim cid pL pT w h
  where integer = rd <$> many digit
        rd = read :: String -> Int

textile :: [Claim] -> Map.Map (Int, Int) Int
textile cs = go cs Map.empty
  where
    go [] mp = mp
    go (x:xs) mp = go xs (newdict x mp)
    newdict c mp = foldr (\pos mp -> Map.insertWith (+) pos 1 mp) mp (lpos c)
    lpos = \c -> (,) <$> [(paddingLeft c)..(width c + paddingLeft c - 1)]
                     <*> [(paddingTop  c)..(height c + paddingTop c - 1)]

overlaps :: [Claim] -> Int
overlaps cs = length $ Map.filter (> 1) (textile cs)

one :: [Claim] -> Int
one cs = foldr (\(b, c) zero -> if b then cid c else zero) 0 $ zipWith (,) (map (isOverlapped tx) cs) cs
  where tx = textile cs
        isOverlapped tx c = all id $ map (1==) $ posValue tx c :: Bool
        posValue tx c = catMaybes $ map (\p -> Map.lookup p tx) (lpos c) :: [Int]
        lpos = \c -> (,) <$> [(paddingLeft c)..(width c + paddingLeft c - 1)]
                     <*> [(paddingTop  c)..(height c + paddingTop c - 1)] :: [(Int, Int)]


main :: IO ()
main = do
  contents <- getContents
  let eitherClaims = fmap (parse claim "") (lines contents)
  putStrLn $ "Star 1: " ++ (show $ overlaps (rights eitherClaims))
  putStrLn $ "Star 2: " ++ (show $ one (rights eitherClaims))
  return ()
