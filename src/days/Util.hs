module Util where

import Data.List

reverse2D :: [[a]] -> [[a]]
reverse2D = map reverse . reverse

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn delim s = reverse2D $ splitOn' [[]] delim s where
    splitOn' cur@(top:ts) (d:ds) (x:xs)
        | d == x = splitOn' cur ds xs
        | otherwise = splitOn' ((x:top) : ts) delim xs
    splitOn' cur [] xs = splitOn' ([] : cur) delim xs
    splitOn' cur _ [] = cur

update :: Eq a => a -> (Maybe b -> b) -> [(a,b)] -> [(a,b)]
update x f ls
    | x `elem` keys = changeVal <$> ls
    | otherwise = (x, f Nothing) : ls
    where
        keys = fst <$> ls
        changeVal (a,b) = (a, if a == x then f $ Just b else b)

countIn :: Eq a => a -> [a] -> Int
countIn x = length . filter (==x)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just $ head xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs
