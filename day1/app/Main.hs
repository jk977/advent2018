module Main where

import Data.List
import Lib

getChanges :: IO [Int -> Int]
getChanges = getContents >>= return . map parseChange . init . splitOn "\n"

part1 :: IO ()
part1 = getChanges >>= print . last . getFreqs 0

part2 :: IO ()
part2 = getChanges >>= print . firstDup . getFreqs 0 . cycle

main :: IO ()
main = part2
