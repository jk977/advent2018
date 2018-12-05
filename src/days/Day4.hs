module Day4 where

import Control.Monad
import Data.Either (rights)
import Data.List (sortOn)
import Text.Parsec

import Util

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

data Action = Sleep | Wake | StartShift deriving Show

data Guard = Id Int | ImplicitId deriving Show

data Log = Log {
    guard :: Guard,
    action :: Action,
    dateTime :: DateTime
} deriving Show

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

part1 :: IO ()
part1 = do
    logs <- sortOn dateTime <$> readLogs
    putStrLn . unlines . map show $ logs

part2 :: IO ()
part2 = undefined
