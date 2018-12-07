module Day5 where

import Data.Char (toLower)
import Data.Function (on)
import Data.List

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
    react' prev s = reverse prev ++ s -- final result

removeUnit :: Char -> String -> String
removeUnit u = filter (not . (~=~u))

part1 :: IO ()
part1 = getLine >>= print . length . react

part2 :: IO ()
part2 = do
    polymer <- getLine

    let units = nubBy (~=~) polymer
        removals = removeUnit <$> units <*> pure polymer
        results = react <$> removals
        best = minimumBy (compare `on` length) results

    print $ length best
