module Util where

import Control.Arrow
import Data.List
import GHC.Exts
import Text.Parsec

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

rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

rotations :: [a] -> [[a]]
rotations xs = take (length xs) . iterate rotate $ xs

readInt :: String -> Int
readInt = read

-- parse an integer of length n
nInt :: Monad m => Int -> ParsecT String u m Int
nInt = fmap readInt . flip count digit

-- combines duplicate keys in lookup table
combineOn :: (Eq a, Ord a) => (b -> c -> c) -> c -> [(a,b)] -> [(a,c)]
combineOn f x0 =
    map (second $ foldr f x0)
    . map (second $ map snd)
    . map (\ls@((x,_):_) -> (x,ls))
    . groupWith fst

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:y:ys) = (x,y) : pairs ys
