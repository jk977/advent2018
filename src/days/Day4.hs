module Day4 where

import Control.Arrow (second)
import Control.Monad (forM)
import Control.Monad.ST

import Data.Time
import Data.STRef

import Data.Either (rights)
import Data.Function (on)
import Data.List

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

isImplicit :: Guard -> Bool
isImplicit ImplicitId = True
isImplicit _          = False

todToMinutes :: TimeOfDay -> Int
todToMinutes t = todMin t + (todHour t * 60)

-- assumes dates are on the same day
minutesBetween :: DateTime -> DateTime -> [Int]
minutesBetween start end = [minute start..minute end]

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
getSleepMins :: [Log] -> [(Guard, [Int])]
getSleepMins = combineOn (++) []
    . map extract
    . pairs
    . filter ((/=StartShift) . action)
    where
        extract (Log g _ dt1, Log _ _ dt2@(DateTime _ min)) =
            let end = dt2{ minute = min-1 } -- -1 to account for waking up
            in  (g, minutesBetween dt1 end)

part1 :: IO ()
part1 = do
    logs <- readLogs

    let sleeps = getSleepMins logs
        (Id slacker, mins) = last . sortOn (length . snd) $ sleeps
        ordered = sortOn (`countIn` mins) mins
        targetMin = last ordered

    print . show $ slacker * targetMin

part2 :: IO ()
part2 = do
    logs <- readLogs

    let freqs = second frequencies <$> getSleepMins logs        :: [(Guard, [(Int,Int)])]
        maxes = second (maximumBy (compare `on` snd)) <$> freqs :: [(Guard, (Int,Int))]
        (Id slacker, (targetMin, _)) = maximumBy (compare `on` (snd . snd)) maxes

    print . show $ slacker * targetMin
