module Day6 where

import Data.Function (on)
import Util

enclose :: [Point] -> Rect
enclose ps = Rect (Point xMin yMin) (Point xMax yMax) where
    extremaOn f = [minimum, maximum] <*> [f <$> ps]
    [xMin, xMax] = extremaOn x
    [yMin, yMax] = extremaOn y

borderPoints :: [Point] -> [Point]
borderPoints ps = nub $ concat [xExtrema, yExtrema] where
    bordersOn f = [minimumBy, maximumBy] <*> pure (compare `on` f) <*> [ps]
    xExtrema = bordersOn x
    yExtrema = bordersOn y

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
