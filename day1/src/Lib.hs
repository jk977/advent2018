module Lib where

parseOp :: Char -> Int -> Int -> Int
parseOp c = case c of
    '+' -> (+)
    '-' -> (-)

parseChange :: String -> Int -> Int
parseChange (op:num) = parseOp op $ read num

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn delim s = map reverse . reverse $ splitOn' [[]] delim s where
    splitOn' cur@(top:ts) (d:ds) (x:xs)
        | d == x = splitOn' cur ds xs
        | otherwise = splitOn' ((x:top) : ts) delim xs
    splitOn' cur [] xs = splitOn' ([] : cur) delim xs
    splitOn' cur _ [] = cur

getFreqs :: Int -> [Int -> Int] -> [Int]
getFreqs cur [] = return cur
getFreqs cur (f:fs) = cur : getFreqs (f cur) fs

update :: (Eq a, Num b) => a -> (b -> b) -> [(a,b)] -> [(a,b)]
update x f ls
    | x `elem` keys = incKey <$> ls
    | otherwise = (x, f 0) : ls
    where
        keys = map fst ls
        incKey (a,b) = (a, if a == x then f b else b)

firstDup :: Eq a => [a] -> Maybe a
firstDup ls = firstDup' [] ls where
    firstDup' _ [] = Nothing
    firstDup' counts (x:xs)
        | lookup x counts > pure 0 = Just x
        | otherwise = firstDup' (update x succ counts) xs
