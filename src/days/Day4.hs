module Day4 where

import Control.Monad (forM)
import Control.Monad.ST

import Data.STRef
import Data.Either (rights)
import Data.List (sortOn)

import Text.Parsec

import Util (readInt, nInt)

data Date = Date {
    year :: Int,
    month :: Int,
    day :: Int
} deriving (Show, Eq, Ord)

data Time = Time {
    hour :: Int,
    minute :: Int
} deriving (Show, Eq, Ord)

data DateTime = DateTime {
    date :: Date,
    time :: Time
} deriving (Show, Eq, Ord)

data Action = Sleep | Wake | StartShift deriving (Show, Eq)

data Guard = Id Int | ImplicitId deriving (Show, Eq)

data Log = Log {
    guard :: Guard,
    action :: Action,
    dateTime :: DateTime
} deriving (Show, Eq)

isImplicit :: Guard -> Bool
isImplicit ImplicitId = True
isImplicit _          = False

minsInHr    = 60
minsInDay   = 24 * minsInHr
minsInYr    = 365 * minsInDay

daysInMo :: Int -> Int
daysInMo n
    | n == 2              = 28
    | n `elem` [4,6,9,11] = 30
    | otherwise           = 31

minsInDate :: Date -> Int
minsInDate d = yr + mo + dy where
    yr = minsInYr * year d
    mo = sum $ (minsInDay*) . daysInMo <$> [1..month d]
    dy = minsInDay * day d

minsInTime :: Time -> Int
minsInTime t = minsInHr * (hour t) + (minute t)

minsInDT :: DateTime -> Int
minsInDT (DateTime d t) = minsInDate d + minsInTime t

timestamp :: Monad m => ParsecT String u m DateTime
timestamp = between (char '[') (char ']') $ do
    yr <- nInt 4
    char '-'
    mo <- nInt 2
    char '-'
    dy <- nInt 2
    space
    hr <- nInt 2
    char ':'
    mn <- nInt 2

    let d = Date yr mo dy
        t = Time hr mn

    return $ DateTime d t

sleep :: Monad m => ParsecT String u m (Guard, Action)
sleep = string "falls asleep" >> return (ImplicitId, Sleep)

wake :: Monad m => ParsecT String u m (Guard, Action)
wake = string "wakes up" >> return (ImplicitId, Wake)

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

readLogs :: IO [Log]
readLogs = getContents
    >>= return
        . rights
        . map (parse logLine "readLogs")
        . lines

-- assumes logs are already sorted by date, and first guard has an ID
convertImplicits :: [Log] -> [Log]
convertImplicits logs = runST $ do
    let firstGuard = guard $ head logs
    lastRef <- newSTRef firstGuard

    forM logs $ \log@(Log g _ _) -> do
        last <- readSTRef lastRef

        if isImplicit g then
            return $ log { guard = last }
        else do
            writeSTRef lastRef g
            return log

part1 :: IO ()
part1 = do
    logs <- convertImplicits . sortOn dateTime <$> readLogs
    putStrLn . unlines . map show $ logs

part2 :: IO ()
part2 = undefined
