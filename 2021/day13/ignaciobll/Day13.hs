{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day13 where

import NeatInterpolation (text)

import Data.Char (isLower)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Set (Set)
import qualified Data.Set as Set

testInput :: Text
testInput =
    [text|
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
|]

newtype Paper = Paper
    { dots :: Set (Int, Int)
    }
    deriving (Show, Eq)

data Instruction = Y Int | X Int deriving (Show, Eq, Read)

data State = State
    { paper :: Paper
    , instructions :: [Instruction]
    }
    deriving (Show, Eq)

parse :: Text -> State
parse input =
    let [rawDots, rawInstructions] = T.splitOn "\n\n" input
        parseDot line = (\[x, y] -> (read $ T.unpack x, read $ T.unpack y)) $ T.splitOn "," line
        paper = Paper $ Set.fromList $ parseDot <$> T.lines rawDots
        parseInstruction i = case T.head $ T.reverse i of
            'x' -> X
            _ -> Y
        parseInstructionLine line = (\[l, n] -> parseInstruction l (read $ T.unpack n)) $ T.splitOn "=" line
        instructions = parseInstructionLine <$> T.lines rawInstructions
     in State paper instructions

foldPoint :: Instruction -> (Int, Int) -> (Int, Int)
foldPoint (X z) (x, y)
    | x > z = (z - (x - z), y)
    | otherwise = (x, y)
foldPoint (Y z) (x, y)
    | y > z = (x, z - (y - z))
    | otherwise = (x, y)

foldAlong :: Instruction -> Paper -> Paper
foldAlong i = Paper . Set.fromList . fmap (foldPoint i) . Set.toList . dots

step :: State -> State
step s@(State _ []) = s
step (State p (i : is)) = State (foldAlong i p) is

fixPoint :: Eq a => (a -> a) -> a -> a
fixPoint f x = let x' = f x in if x' == x then x else fixPoint f x'

star1 :: State -> Int
star1 = Set.size . dots . paper . step

showPaper :: Paper -> Text
showPaper paper =
    let (maxX, maxY) = Set.foldr (\(x, y) (x', y') -> (max x x', max y y')) (0, 0) . dots $ paper
        serial :: [String]
        serial = [[if Set.member (x, y) (dots paper) then '█' else ' ' | x <- [0 .. maxX]] | y <- [0 .. maxY]]
     in T.unlines $ fmap T.pack serial

star2 :: State -> Text
star2 = showPaper . paper . fixPoint step

main :: IO ()
main = do
    input <- parse <$> T.readFile "input"
    putStrLn $ "Test 1 " ++ show (star1 (parse testInput))
    putStrLn $ "Star 1 " ++ show (star1 input)
    T.putStrLn $ "Test 2:\n" <> star2 (parse testInput)
    T.putStrLn $ "Star 2:\n" <> star2 input
