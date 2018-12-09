module Day6 where

import Data.List (sortOn)
import Util (Point(..), subPt)

manhattanDistance :: Point -> Point -> Int
manhattanDistance p1 p2 = abs x + abs y where
    Point x y = subPt p1 p2

readPoint :: String -> Point
readPoint s = Point x y where
    [(x':_), y'] = words s
    (x,y) = (read [x'], read y')

part1 :: IO ()
part1 = do
    points <- map readPoint . lines <$> getContents
    return ()

part2 :: IO ()
part2 = undefined
