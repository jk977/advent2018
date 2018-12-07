module Day5 where

import Data.Char

(~=~) :: Char -> Char -> Bool
a ~=~ b = (toLower a) == (toLower b)

react' :: String -> String -> String
react' prev (x:y:ys)
    | not (x ~=~ y) || (x == y) = react' (x:prev) (y:ys)
    | otherwise = react' [] (reverse prev ++ ys)
react' prev s = prev ++ s

react :: String -> String
react = react' []

part1 :: IO ()
part1 = getLine >>= print . length . react

part2 :: IO ()
part2 = undefined
