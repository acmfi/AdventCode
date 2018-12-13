import Text.ParserCombinators.ReadP
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Data.Map.Strict (Map, fromList, (!))

type Pot = Int
type Env = [Bool]
type Rule = (Env,Bool)

readInit :: ReadP [Pot]
readInit = do
  _ <- string "initial state: "
  readPots

readRule :: ReadP Rule
readRule = do
  l <- readPre
  _ <- string " => "
  to <- get
  return $ (l,(potCon to))

readPre :: ReadP [Bool]
readPre = do
  let isPot x = x == '#' || x == '.'
      int t = foldl (\l c -> (potCon c):l) [] t
  t <- munch1 isPot
  return $ int t

readPots :: ReadP [Int]
readPots = do
  let isPot x = x == '#' || x == '.'
      int t = foldr (\(n,c) -> if c=='#' then (n:) else id) [] (zip [0..] t)
  t <- munch1 isPot
  return $ int t

runP  :: ReadP a -> String -> a
runP parser str = readP_to_S parser str & last & fst

potCon :: Char -> Bool
potCon '.' = False
potCon '#' = True
potCon _ = undefined

-- | Problem solving

takeEnv :: [Int] -> Int -> Env
takeEnv l s = map (\x -> elem x l) [(s+2),(s+1)..(s-2)]

applyRule :: Map Env Bool -> [Pot] -> [Pot]
applyRule r p = foldl (\l i -> if r ! (takeEnv p i) then i:l else l) [] [fr..to]
  where fr = (minimum p) - 2
        to = (maximum p) + 2

star1 :: Map Env Bool -> [Pot] -> Int
star1 rules initial = sum $ it rules initial 20

it ::  Map Env Bool -> [Pot] -> Int -> [Pot]
it r p i = foldl (\q _ -> applyRule r q) p (replicate i False)

-- star2 :: Map Env Bool -> [Pot] -> Int
-- star2 rules initial = (start,end) --getN $ c !! (start -1 + mod (50000000000 - start) period)
--   where (start,end,c) = getPeriod rules initial []
--         period = start-end

star2 :: Map Env Bool -> [Pot] -> Int
star2 rules initial = sum $ it rules initial 10000


-- getPeriod :: Map Env Bool -> [Pot] -> [[Pot]] -> (Int, Int, [[Pot]])
-- getPeriod r p done = if any (similar next) done then (fromJust $ elemIndex next done, 0, done) else (\(a,b,c) -> (a,b+1,c)) $ getPeriod r next (next:done)
--   where next = applyRule r p


similar :: [Pot] -> [Pot] -> Bool
similar p q = all (flip elem q) p && all (flip elem p) q


main :: IO ()
main = do
  text <- readFile "input"
  let (initialS:_:rulesS) = lines text
  let initial = runP readInit initialS
  let rules = fromList $ map (runP readRule) rulesS
  -- putStrLn $ show $ initial
  --putStrLn $ show $ rules
  putStrLn $ show $ star1 rules initial
  putStrLn $ show $ star2 rules initial
  --putStrLn $ show $ (\(a,b,_) -> (a,b)) $ getPeriod rules initial []
  return ()
