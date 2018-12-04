{-# LANGUAGE FlexibleContexts #-}

import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.LocalTime (TimeOfDay(..), timeToTimeOfDay)
import Data.Time.Clock (UTCTime(..), DiffTime(..), secondsToDiffTime)
import Text.Parsec
import Text.Parsec.Char
import Control.Applicative hiding (many, (<|>))
import Data.List (sort)
import Data.Either (rights)
import Data.Tuple (swap)
import qualified Data.IntMap.Strict as Map

data Event = BeginShift Int | Asleep | WakeUp deriving (Eq, Show)

data Record = Record
  { date :: UTCTime
  , event :: Event
  } deriving (Eq, Show)

instance Ord Record where
  (Record d _) `compare` (Record d' _) = d `compare` d'

pDate :: Stream s m Char => ParsecT s u m UTCTime
pDate = do
  char '['
  year <- rd <$> count 4 digit
  char '-'
  month <- rdInt <$> count 2 digit
  char '-'
  day <- rdInt <$> count 2 digit
  char ' '
  hour <- rd <$> count 2 digit
  string ":"
  minute <- rd <$> count 2 digit
  string "] "
  return $ UTCTime (fromGregorian year month day) (secondsToDiffTime $ hour * 3600 + minute * 60)
  where rd = read :: String -> Integer
        rdInt = read :: String -> Int

pShift, pAsleep, pWakeUp, pEvent :: Stream s m Char => ParsecT s u m Event
pShift = do
  string "Guard #"
  gid <- rd <$> many digit
  string " begins shift"
  return $ BeginShift gid
  where rd = read :: String -> Int

pAsleep = do
  string "falls asleep"
  return Asleep

pWakeUp = do
  string "wakes up"
  return WakeUp

pEvent = do
  event <- pShift <|> pAsleep <|> pWakeUp
  return event

pRecord :: Stream s m Char => ParsecT s u m Record
pRecord = do
  d <- pDate
  event <- pEvent
  return $ Record d event

-- END OF PARSING --
--                         id           h   times
type RecordSheet = Map.IntMap (Map.IntMap Int)

track :: [Record] -> RecordSheet      --id  h  acc
track records = trd $ foldl (flip addRecord) (0, 0, Map.empty) records
  where trd = \(_,_, x) -> x

-- all asleep/awake times are during the midnight hour (00:00 - 00:59)
addRecord :: Record -> (Int, Int, RecordSheet) -> (Int, Int, RecordSheet)
addRecord (Record time event) (gid, sleepMinute, rs) =
  case event of
    BeginShift gid' -> (gid', 0, rs)
    Asleep -> (gid, getMinute time, rs)
    WakeUp -> (gid, 0, incrementSleepRangeOfGid gid rs [sleepMinute..wakeUpMinute-1])
  where wakeUpMinute = getMinute time

incrementSleepRangeOfGid :: Int          -- Guard ID
                         -> RecordSheet  -- Registro general
                         -> [Int]        -- Minutos en los que está dormido
                         -> RecordSheet  -- Registro general actualizado
incrementSleepRangeOfGid gid rs minutes = Map.insert gid updateRS rs
  where updateRS = incrementSleepRange (Map.findWithDefault Map.empty gid rs) minutes

incrementSleepRange :: Map.IntMap Int -> [Int] -> Map.IntMap Int
incrementSleepRange rs = foldr incrementSleep rs

incrementSleep :: Int -> Map.IntMap Int -> Map.IntMap Int
incrementSleep minute m = Map.insertWith (+) minute 1 m

getHour, getMinute :: UTCTime -> Int
getHour (UTCTime _ seconds) = todHour $ timeToTimeOfDay seconds
getMinute (UTCTime _ seconds) = todMin $ timeToTimeOfDay seconds

star1 :: [Record] -> Int
star1 rs = gid * minute
  where (_, gid) = foldr max (0,0) $ map swap $ Map.toList $ Map.map (foldr (+) 0) $ track rs
        (Just minute) = fmap (snd . (foldr max (0,0)) . map swap . Map.toList) $ Map.lookup gid $ track rs

star2 :: [Record] -> Int
star2 rs = gid * minute
  where list = Map.toList $ Map.map ((foldr max (0,0)) . map swap . Map.toList) $ track rs
        ltuples = map (\(a, (b, c)) -> (b, (a, c))) list
        (gid, minute) = snd . (foldr max (0,(0,0))) $ ltuples


main :: IO ()
main = do
  contents <- getContents
  let sorted = sort . rights . fmap (parse pRecord "") $ (lines contents)
  putStrLn $ "Star 1: " ++ (show $ star1 sorted)
  putStrLn $ "Star 2: " ++ (show $ star2 sorted)
