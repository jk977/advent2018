module Util where

import Control.Arrow (second)
import Control.Applicative (liftA2)

import Data.List
import Data.Function (on)

import GHC.Exts
import Text.Parsec

data Point = Point {
    x :: Int,
    y :: Int
} deriving (Show, Ord, Eq)

data Line = Line {
    start :: Point,
    end :: Point
} deriving (Show, Ord, Eq)

data Rect = Rect {
    vertexA :: Point,
    vertexB :: Point
} deriving (Show, Ord, Eq)

onPt :: (Int -> Int -> Int) -> Point -> Point -> Point
onPt f (Point a b) (Point c d) = Point (f a c) (f b d)

addPt :: Point -> Point -> Point
addPt = onPt (+)

subPt :: Point -> Point -> Point
subPt = onPt (-)

enclose :: [Point] -> Rect
enclose ps = Rect (Point xMin yMin) (Point xMax yMax) where
    extremaOn f = [minimum, maximum] <*> [f <$> ps]
    [xMin, xMax] = extremaOn x
    [yMin, yMax] = extremaOn y

dimensions :: Rect -> (Int,Int)
dimensions (Rect p1 p2) = (abs diffX, abs diffY) where
    Point diffX diffY = subPt p1 p2

pointsIn :: Rect -> [Point]
pointsIn (Rect a b) = [Point x y | x <- [x a..x b], y <- [y a..y b]]

pointsInAll :: [Rect] -> [Point]
pointsInAll = nub . concatMap pointsIn

-- rectangles overlap iff (x a1) <= (x b2), (x a2) <= (x b1), ...
rectsOverlap :: Rect -> Rect -> Bool
rectsOverlap (Rect a1 b1) (Rect a2 b2) = and $ fs args where
    fs = liftA2 uncurry [(<=) `on` x, (<=) `on` y]
    args = [(a1,b2), (a2,b1)]

-- points that lie in both rects
overlapRect :: Rect -> Rect -> [Point]
overlapRect r1@(Rect a1 b1) r2@(Rect a2 b2)
    | rectsOverlap r1 r2 = pointsIn r1 `intersect` pointsIn r2
    | otherwise = []

update :: Eq a => a -> (Maybe b -> b) -> [(a,b)] -> [(a,b)]
update x f ls
    | x `elem` keys = changeVal <$> ls
    | otherwise     = (x, f Nothing) : ls
    where
        keys = fst <$> ls
        changeVal (a,b) = (a, if a == x then f $ Just b else b)

countIn :: Eq a => a -> [a] -> Int
countIn x = length . filter (==x)

frequencies :: Eq a => [a] -> [(a, Int)]
frequencies xs = nub . zip xs $ (`countIn` xs) <$> xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just $ head xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

rotate :: [a] -> [a]
rotate []     = []
rotate (x:xs) = xs ++ [x]

rotations :: [a] -> [[a]]
rotations xs = take (length xs) . iterate rotate $ xs

readInt :: String -> Int
readInt = read

-- parse an integer of length n
nInt :: Monad m => Int -> ParsecT String u m Int
nInt n = readInt <$> count n digit

-- combines duplicate keys in lookup table
combineOn :: (Eq a, Ord a) => (b -> c -> c) -> c -> [(a,b)] -> [(a,c)]
combineOn f x0 = map (second $ foldr f x0)
    . map (second $ map snd)
    . map (\ls@((x,_):_) -> (x,ls))
    . groupWith fst

pairs :: [a] -> [(a,a)]
pairs []       = []
pairs (x:y:ys) = (x,y) : pairs ys
