module Lib where

parseChange :: String -> Int -> Int
parseChange (c:num) = op $ read num where
    op = case c of
        '+' -> (+)
        '-' -> subtract

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

firstDup :: Eq a => [a] -> Maybe a
firstDup ls = firstDup' [] ls where
    firstDup' _ [] = Nothing
    firstDup' counts (x:xs)
        | lookup x counts > pure 0 = Just x
        | otherwise = firstDup' (update x (maybe 1 succ) counts) xs

getFreqs :: Int -> [Int -> Int] -> [Int]
getFreqs cur [] = return cur
getFreqs cur (f:fs) = cur : getFreqs (f cur) fs
