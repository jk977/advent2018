module Day2 where

import Data.List
import Data.Function
import Util

containsNReps :: Eq a => Int -> [a] -> Bool
containsNReps n ls = not . null $ matches where
    counts = countIn <$> ls <*> pure ls
    matches = filter (==n) counts

countNReps :: Eq a => Int -> [[a]] -> Int
countNReps n = length . filter (containsNReps n)

checksum :: [String] -> Int
checksum ls = (countNReps 2 ls) * (countNReps 3 ls)

partitionPairs :: [String] -> [(String,String)]
partitionPairs bs = do
    b1 <- bs
    b2 <- filter (/=b1) bs

    let comps = zipWith (==) b1 b2
        tagged = zip b1 comps
        (matches,diffs) = partition snd tagged
        stripped = (fst <$> matches, fst <$> diffs)

    return stripped

findBox :: [String] -> String
findBox = head . map fst . sortBy (compare `on` length . snd) . partitionPairs

part1 :: IO ()
part1 = getContents >>= print . checksum . lines

part2 :: IO ()
part2 = getContents >>= putStrLn . findBox . lines
