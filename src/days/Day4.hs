module Day4 where

import Control.Arrow (second)
import Control.Monad (forM)
import Control.Monad.ST

import Data.Time
import Data.Fixed

import Data.STRef
import Data.Either (rights)
import Data.List (sortOn)

import GHC.Exts (groupWith)
import Text.Parsec
import Debug.Trace

import Util

type DateTime = UTCTime

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

parseDT :: String -> DateTime
parseDT = parseTimeOrError False defaultTimeLocale "%Y-%-m-%-d %H:%M"

timestamp :: Monad m => ParsecT String u m DateTime
timestamp = between (char '[') (char ']') $ parseDT <$> many (noneOf "]")

sleep :: Monad m => ParsecT String u m (Guard, Action)
sleep = string "falls asleep" >> return (ImplicitId, Sleep)

wake :: Monad m => ParsecT String u m (Guard, Action)
wake = string "wakes up" >> return (ImplicitId, Wake)

startShift :: Monad m => ParsecT String u m (Guard, Action)
startShift =
    string "Guard #"
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
readLogs =
    getContents
    >>= return
        . rights
        . map (parse logLine "readLogs")
        . lines

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

-- assumes logs are sorted by date, and that every sleep log
-- has a corresponding wake log
getSleepTimes :: [Log] -> [(Guard, [TimeRange])]
getSleepTimes =
    combineOn (:) []
    . map (\(Log g _ dt1, Log _ _ dt2) -> (g, TimeRange dt1 dt2))
    . pairs
    . filter ((/=StartShift) . action)

part1 :: IO ()
part1 = do
    logs <- convertImplicits . sortOn dateTime <$> readLogs
    let times = getSleepTimes logs
    putStrLn . unlines . map show $ times

part2 :: IO ()
part2 = undefined
