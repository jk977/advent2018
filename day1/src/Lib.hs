module Lib where

parseOp :: Char -> Int -> Int -> Int
parseOp c = case c of
    '+' -> (+)
    '-' -> subtract

parseChange :: String -> Int -> Int
parseChange (op:num) = parseOp op $ read num

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn delim s = map reverse $ reverse (splitOn' [[]] delim s) where
    splitOn' cur@(top:ts) (d:ds) (x:xs)
        | d == x = splitOn' cur ds xs
        | otherwise = splitOn' ((x:top) : ts) delim xs
    splitOn' current [] xs = splitOn' ([]:current) delim xs
    splitOn' current _ [] = current
