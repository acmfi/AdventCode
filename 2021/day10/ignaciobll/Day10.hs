{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day10 where

import NeatInterpolation (text)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Either (rights)
import Data.List (sort)

testInput :: Text
testInput =
    [text|
[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
|]

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
score _ = 0

inverse :: Char -> Char
inverse '(' = ')'
inverse '[' = ']'
inverse '{' = '}'
inverse '<' = '>'
inverse x = x

parse :: Text -> [String]
parse = fmap T.unpack . T.lines

matchSyntax :: [Char] -> (Char, [Char])
matchSyntax = go []
  where
    go :: [Char] -> [Char] -> (Char, [Char])
    go [] [] = (' ', [])
    go ('(' : xs) (')' : ys) = go xs ys
    go ('[' : xs) (']' : ys) = go xs ys
    go ('{' : xs) ('}' : ys) = go xs ys
    go ('<' : xs) ('>' : ys) = go xs ys
    go xs (y : ys)
        | y `elem` ("([{<" :: String) = go (y : xs) ys
        | otherwise = (y, xs)
    go (x : xs) [] = (x, x : xs)

findMismatch :: String -> Char
findMismatch = fst . matchSyntax

star1 :: [String] -> Int
star1 = sum . fmap (score . findMismatch)

guessCompletion :: String -> Either String String
guessCompletion str = case matchSyntax str of
    (n, stack)
        | score n == 0 -> Right (inverse <$> stack)
        | otherwise -> Left (inverse <$> stack)

score2Char :: Char -> Int
score2Char ')' = 1
score2Char ']' = 2
score2Char '}' = 3
score2Char '>' = 4
score2Char _ = 0

score2 :: String -> Int
score2 = go 0
  where
    go :: Int -> String -> Int
    go n [] = n
    go n (x : xs) = go ((5 * n) + score2Char x) xs

star2 :: [String] -> Int
star2 input =
    let completions = rights $ fmap guessCompletion input
        scores = fmap score2 completions
        middle xs = xs !! (length xs `div` 2)
        midScore = middle (sort scores)
     in midScore

main :: IO ()
main = do
    input <- parse <$> T.readFile "input"
    putStrLn $ "Test 1 " ++ show (star1 (parse testInput))
    putStrLn $ "Star 1 " ++ show (star1 input)
    putStrLn $ "Test 2 " ++ show (star2 (parse testInput))
    putStrLn $ "Test 2 " ++ show (star2 input)
