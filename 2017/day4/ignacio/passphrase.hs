import Data.Set (fromList, toList)
import System.IO (getContents)

correctPassPhrase :: String -> Bool
correctPassPhrase pp = length ppw == length pp
    where ppw = unwords . toList . fromList . words $ pp


correctPassPhrase' :: String -> Bool
correctPassPhrase' pp = (length. fromList $ ppw) == (length ppw)
    where ppw = words pp


nCorrects = length . filter id . map correctPassPhrase

nCorrects' lpp = foldr (countCorrect) 0 lpp
    where countCorrect pp c = if correctPassPhrase' pp then c + 1 else c


test1 = ["aa bb cc dd ee", "aa bb cc dd aa", "aa bb cc dd aaa"]


main :: IO ()
main = do
  input <- getContents
  putStrLn . show . nCorrects' . lines $ input
