#!/usr/bin/env stack
-- stack --resolver lts-16.24 script --package split

import           Data.List.Split                ( wordsBy )
import           Data.List                      ( sort )

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn $ "Star 1: " ++ (show $ star1 input)
  print input

parseInput :: String -> (Int, [Int])
parseInput input =
  let (arrive : busLines) = lines input
      buses               = fmap read . filter (/= "x") . wordsBy (== ',') . head
  in  (read arrive, buses busLines)

star1 :: (Int, [Int]) -> Int
star1 (start, buses) =
  let busesStart = sort $ map
        (\b -> (let d = div start b in if d * b < start then d * b + b else d * b, b))
        buses
      (busStart, busId) = head busesStart
  in  (busStart - start) * busId
