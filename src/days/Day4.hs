module Day4 where

import Control.Monad (forM)
import Control.Monad.ST

import Data.Time
import Data.STRef
import Data.Either (rights)
import Data.List (sortOn)

import Text.Parsec

import Util

type DateTime = UTCTime

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

parseDT :: String -> DateTime
parseDT = parseTimeOrError False defaultTimeLocale "%Y-%-m-%-d %H:%M"

timestamp :: Monad m => ParsecT String u m DateTime
timestamp = between (char '[') (char ']') $ parseDT <$> many (noneOf "]")

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
    print logs

part2 :: IO ()
part2 = undefined
