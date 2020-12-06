data Tree = Empty | Garbage String | Group [Tree]

instance Show Tree where
  show Empty = "{}"
--  show (Garbage a) = "<" ++ a ++ ">"
  show (Garbage _) = ""
  show (Group children) = "{" ++ let (x:xs) = map show children in foldr (\c acc -> acc ++ "," ++ c) x xs ++ "}"

score' :: Int -> Tree -> Int
score' n Empty = 1 + n
score' _ (Garbage _) = 0
score' n (Group cs) = 1 + n + sum (map (score' (n+1)) cs)

score :: Tree -> Int
score = score' 0

-- Parser code. Here be dragons
garbage :: String -> (Maybe Tree, String)
garbage [] = (Nothing, ":garbage:")
garbage ('>':xs) = (Just $ Garbage "", xs)
garbage ('!':x:xs) = case garbage xs of
  (Just (Garbage s), xss) -> (Just $ Garbage $ '!':x:s, xss)
  s -> s
garbage (x:xs) = case garbage xs of
  (Just (Garbage s), xss) -> (Just $ Garbage $ x:s, xss)
  s -> s

treeLst :: String -> (Maybe [Tree], String)
treeLst s = case tree s of
  (Nothing, xs) -> (Nothing, xs)
  (Just c, ',':xs) -> case treeLst xs of
    (Just cs, xss) -> (Just $ c:cs, xss)
    (Nothing, xss) -> (Nothing, xss)
  (Just c, xs) -> (Just [c], xs)

tree :: String -> (Maybe Tree, String)
tree ('<':xs) = garbage xs
tree ('{':'}':xs) = (Just Empty, xs)
tree ('{':xs) = case treeLst xs of
  (Just c, '}':xss) -> (Just $ Group c, xss)
  (_, xss) -> (Nothing, xss)
tree s = (Nothing, s)
-- End of parser code

main :: IO ()
main = do
  input <- readFile "input"
  print input
  case tree input of
    (Just t, "") -> do
      putStr "Parsed: "
      print t
      putStr "Score: "
      print $ score t
    s -> print s

