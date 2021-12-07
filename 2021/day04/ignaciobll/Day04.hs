{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day04 where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text)

newtype Board = Board {getBoard :: Map (Int, Int) (Int, Bool)} deriving (Show, Eq)

data Input = Input
    { numbers :: [Int]
    , boards :: [Board]
    }
    deriving (Show)

data BoardGame = BoardGame
    { boardNumbers :: [Int]
    , calledNumbers :: [Int]
    , winningNumber :: Maybe Int
    , board :: Board
    }
    deriving (Show, Eq)

parse :: Text -> Input
parse input =
    let (rawNumbers : rawBoards) = T.splitOn "\n\n" input
        numbers = (read . T.unpack <$> T.splitOn "," rawNumbers)
        parseBoard :: Text -> Board
        parseBoard raw =
            Board $
                M.fromList $
                    zip
                        [(x, y) | x <- [1 .. 5], y <- [1 .. 5]] -- Assign each position in board
                        $ fmap ((,False) . read . T.unpack) $ -- Default false (not selected)
                            mconcat . fmap T.words $ -- Int values of board, flatten
                                T.lines raw -- Rows of board
        boards = parseBoard <$> rawBoards
     in Input numbers boards

markNumber :: Int -> Board -> Board
markNumber n = Board . M.map (\(v, called) -> (v, n == v || called)) . getBoard

isBoardWinner :: Board -> Bool
isBoardWinner (Board board) =
    let columns = [M.size $ M.filterWithKey (\(_, col) (_, called) -> col == x && called) board | x <- [1 .. 5]]
        rows = [M.size $ M.filterWithKey (\(row, _) (_, called) -> row == y && called) board | y <- [1 .. 5]]
        lines = columns ++ rows
     in elem 5 lines

calculateScore :: Int -> Board -> Int
calculateScore lastNumber = (* lastNumber) . M.foldr (+) 0 . M.map fst . M.filter (not . snd) . getBoard

fixPoint :: Eq a => (a -> a) -> a -> a
fixPoint f x = let x' = f x in if x == x' then x else fixPoint f x'

boardGameStep :: BoardGame -> BoardGame
boardGameStep bg@(BoardGame [] _ _ _) = bg
boardGameStep bg@(BoardGame _ _ (Just _) _) = bg
boardGameStep bg@(BoardGame (x : xs) hist _ board) = BoardGame xs (x : hist) winN board'
  where
    board' = markNumber x board
    winN = if isBoardWinner board' then Just x else Nothing

playAllGames :: Input -> Map Int (Int, Board)
playAllGames Input{numbers, boards} = M.fromList $ toBoardPosition . playBoardGame . mkBoardGame numbers <$> boards
  where
    playBoardGame = fixPoint boardGameStep

    mkBoardGame :: [Int] -> Board -> BoardGame
    mkBoardGame n = BoardGame n [] Nothing

    toBoardPosition :: BoardGame -> (Int, (Int, Board))
    toBoardPosition bg@(BoardGame _ called Nothing board) = (length called, (0, board))
    toBoardPosition bg@(BoardGame _ called (Just n) board) = (length called, (n, board))

star1 :: Input -> Int
star1 = uncurry calculateScore . snd . M.findMin . playAllGames

star2 :: Input -> Int
star2 = uncurry calculateScore . snd . M.findMax . playAllGames

main :: IO ()
main = do
    input <- parse <$> T.getContents
    putStrLn $ "Star 1: " ++ show (star1 input)
    putStrLn $ "Star 2: " ++ show (star2 input)
