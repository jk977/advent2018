module Day4 where

import Control.Monad (forM)
import Control.Monad.ST

import Data.Time
import Data.STRef
import Data.Either (rights)
import Data.List (sortOn)

import Text.Parsec
import Util

data DateTime = DateTime {
    day :: Day,
    minute :: Int
} deriving (Show, Eq, Ord)

data Action = Sleep | Wake | StartShift deriving (Show, Eq)

data Guard = Id Int | ImplicitId deriving (Show, Eq, Ord)

data Log = Log {
    guardID :: Guard,
    action :: Action,
    dateTime :: DateTime
} deriving (Show, Eq)

data TimeRange = TimeRange {
    start :: DateTime,
    end :: DateTime
} deriving (Show, Eq)

isImplicit :: Guard -> Bool
isImplicit ImplicitId = True
isImplicit _          = False

todToMinutes :: TimeOfDay -> Int
todToMinutes t = todMin t + (todHour t * 60)

minutesToTod :: Int -> TimeOfDay
minutesToTod = timeToTimeOfDay . fromIntegral . (*60)

minutesIn :: TimeRange -> [Int]
minutesIn (TimeRange s e) = [minute s..minute e]

parseDT :: String -> DateTime
parseDT s = DateTime day minutes where
    parseDT' :: ParseTime t => String -> String -> t
    parseDT' = parseTimeOrError False defaultTimeLocale

    [ymd, time] = words s
    day = parseDT' "%Y-%-m-%-d" ymd
    minutes = todToMinutes . parseDT' "%H:%M" $ time

timestamp :: Monad m => ParsecT String u m DateTime
timestamp = between (char '[') (char ']')
    $ (parseDT <$> many (noneOf "]"))

sleep :: Monad m => ParsecT String u m (Guard, Action)
sleep = string "falls asleep"
    >> return (ImplicitId, Sleep)

wake :: Monad m => ParsecT String u m (Guard, Action)
wake = string "wakes up"
    >> return (ImplicitId, Wake)

startShift :: Monad m => ParsecT String u m (Guard, Action)
startShift = string "Guard #"
    >> (readInt <$> many digit)
    >>= \n -> return (Id n, StartShift)

guardAction :: Monad m => ParsecT String u m (Guard, Action)
guardAction = sleep <|> wake <|> startShift

logLine :: Monad m => ParsecT String u m Log
logLine = do
    dt <- timestamp
    space
    (n, action) <- guardAction
    return $ Log n action dt

-- assumes logs are already sorted by date, and first guard has an ID
convertImplicits :: [Log] -> [Log]
convertImplicits logs = runST $ do
    let firstGuard = guardID $ head logs
    lastRef <- newSTRef firstGuard

    forM logs $ \log@(Log g _ _) -> do
        last <- readSTRef lastRef

        if isImplicit g then
            return $ log { guardID = last }
        else do
            writeSTRef lastRef g
            return log

readLogs :: IO [Log]
readLogs = getContents
    >>= return
        . convertImplicits
        . sortOn dateTime
        . rights
        . map (parse logLine "readLogs")
        . lines

-- assumes logs are sorted by date, and that every sleep log
-- has a corresponding wake log
getSleepTimes :: [Log] -> [(Guard, [TimeRange])]
getSleepTimes = combineOn (:) []
    . map extract
    . pairs
    . filter ((/=StartShift) . action)
    where
        extract (Log g _ dt1, Log _ _ dt2@(DateTime _ m)) =
            (g, TimeRange dt1 dt2{ minute=m-1 }) -- -1 to account for waking up

part1 :: IO ()
part1 = do
    logs <- readLogs

    let (Id slacker, ts) = last $ getSleepTimes logs
        mins = ts >>= minutesIn
        ordered = sortOn (`countIn` mins) mins
        targetMin = last ordered `mod` 60

    putStrLn $ "Sleepiest guard is " ++ (show slacker)
    putStrLn $ "Most commonly asleep at " ++ (show targetMin)
    putStrLn $ "Answer is " ++ (show $ slacker * targetMin)

part2 :: IO ()
part2 = undefined
