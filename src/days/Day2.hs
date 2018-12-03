module Day2 where

import Util

containsNReps :: Eq a => Int -> [a] -> Bool
containsNReps n ls = not . null . filter (==n) $ counts where
    counts = countIn <$> ls <*> pure ls

countNReps :: Eq a => Int -> [[a]] -> Int
countNReps n = length . filter (containsNReps n)

checksum :: [String] -> Int
checksum ls = (countNReps 2 ls) * (countNReps 3 ls)

part1 :: IO ()
part1 = getContents >>= print . checksum . lines
