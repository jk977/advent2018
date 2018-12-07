module Day6 where

import Control.Monad
import Data.Function (on)
import Data.List
import Util

data Status = Taken Point | Attempt Point | Tie | Empty

isTaken :: Status -> Bool
isTaken (Taken _) = True
isTaken _         = False

borderPoints :: [Point] -> [Point]
borderPoints ps = nub $ concat [xExtrema, yExtrema] where
    bordersOn f = [minimumBy, maximumBy] <*> pure (compare `on` f) <*> pure ps
    xExtrema = bordersOn x
    yExtrema = bordersOn y

enclose :: [Point] -> Rect
enclose ps = Rect (Point xMin yMin) (Point xMax yMax) where
    extremaOn f = [minimum, maximum] <*> pure (f <$> ps)
    [xMin, xMax] = extremaOn x
    [yMin, yMax] = extremaOn y

makeGrid :: [Point] -> [(Point, Status)]
makeGrid ps = replace <$> blankGrid where
    blankGrid = flip zip (repeat Empty) . pointsIn . enclose $ ps
    replace cell@(pt, stat) = if pt `elem` ps then (pt, Taken pt) else cell

area :: [Point] -> [(Point, Status)] -> Int
area borders grid = length $ filter (isFinite . snd) filled where
    filled = filter (isTaken . snd) grid
    isFinite (Taken p) = not $ p `elem` borders

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
