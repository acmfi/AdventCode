#!/usr/bin/env stack
-- stack --resolver lts-16.24 script --package containers

import           Data.Char                      ( toUpper )
import           Data.Foldable                  ( toList )

import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IntMap

import           Data.Sequence                  ( Seq(..)
                                                , (|>)
                                                , (><)
                                                )
import qualified Data.Sequence                 as Seq

data Op = ACC Int | JMP Int | NOP Int deriving (Show, Eq, Read)

stateR :: (IntMap (Op, Bool), Int, Int) -> Either Int Int
stateR (code, pc, acc) = case IntMap.lookup pc code of
  Nothing        -> Left acc
  Just (_, True) -> Right acc
  Just (op, _) ->
    let code' = IntMap.update (const $ Just (op, True)) pc code
    in  case op of
          (ACC n) -> stateR (code', pc + 1, acc + n)
          (JMP n) -> stateR (code', pc + n, acc)
          (NOP _) -> stateR (code', pc + 1, acc)

generateCodes :: [Op] -> [[Op]]
generateCodes code = code : (fmap toList $ go Seq.empty (Seq.fromList code))
 where
  go :: Seq Op -> Seq Op -> [Seq Op]
  go prev Empty      = []
  go prev (x :<| xs) = case x of
    (JMP n)   -> ((prev |> (NOP n)) >< xs) : go (prev |> x) xs
    (NOP n)   -> ((prev |> (JMP n)) >< xs) : go (prev |> x) xs
    otherwise -> go (prev |> x) xs

star1 :: [Op] -> Either Int Int
star1 code = stateR (mkSource code, 0, 0) -- Ignore Left or Right, just get the acc

star2 :: [Op] -> Either Int [Int]
star2 = traverse (\code' -> stateR (mkSource code', 0, 0)) . generateCodes

mkSource :: [Op] -> IntMap (Op, Bool)
mkSource ops = IntMap.fromList (zip [0 ..] $ zip ops (repeat False))

main :: IO ()
main = do
  input <- (fmap read . lines . filter (/= '+') . fmap toUpper) <$> getContents :: IO [Op]
  putStrLn $ "Star 1: " ++ show (star1 input)
  putStrLn $ "Star 2: " ++ show (star2 input)
