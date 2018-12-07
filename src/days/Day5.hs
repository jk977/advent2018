module Day5 where

import Data.Char

(~=~) :: Char -> Char -> Bool
a ~=~ b = (toLower a) == (toLower b)

react :: String -> String
react = react' [] where
    -- prev tracks the previous items so when a change happens, the reaction
    -- can start from the beginning again. prev is in reverse to prevent
    -- list concatenation every time no reaction occurs
    react' prev (x:y:ys)
        | not (x ~=~ y) || (x == y) = react' (x:prev) (y:ys) -- no reaction
        | otherwise                 = react' [] (reverse prev ++ ys)
    react' prev s = prev ++ s -- final result

part1 :: IO ()
part1 = getLine >>= print . length . react

part2 :: IO ()
part2 = undefined
