module Day1 where

import Util

getChanges :: IO [Int -> Int]
getChanges = getContents >>= return . map parseChange . init . splitOn "\n"

parseChange :: String -> Int -> Int
parseChange (c:num) = op $ read num where
    op = case c of
        '+' -> (+)
        '-' -> subtract

getFreqs :: Int -> [Int -> Int] -> [Int]
getFreqs cur [] = return cur
getFreqs cur (f:fs) = cur : getFreqs (f cur) fs

firstDup :: Eq a => [a] -> Maybe a
firstDup ls = firstDup' [] ls where
    firstDup' _ [] = Nothing
    firstDup' counts (x:xs)
        | lookup x counts > pure 0 = Just x
        | otherwise = firstDup' (update x (maybe 1 succ) counts) xs

part1 :: IO ()
part1 = getChanges >>= print . last . getFreqs 0

part2 :: IO ()
part2 = getChanges >>= print . firstDup . getFreqs 0 . cycle
