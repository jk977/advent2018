module Day3 where

import Data.List
import Data.Function
import Util

data Point = Point {
    x :: Int,
    y :: Int
} deriving (Show, Ord, Eq)

data Rect = Rect {
    vertexA :: Point,
    vertexB :: Point
} deriving (Show, Ord, Eq)

data Claim = Claim {
    cid :: Int,
    span :: Rect
} deriving (Show, Ord, Eq)

makeRect :: Point -> Int -> Int -> Rect
makeRect start@(Point x y) width height = Rect start $ Point (x+width) (y+height)

area :: Rect -> Int
area (Rect (Point x1 y1) (Point x2 y2)) = (x2 - x1) * (y2 - y1)

parseClaim :: String -> Claim
parseClaim s = Claim cid span where
    readOn d = map read . splitOn d

    [idS,_,ptS,dimS] = words s
    [x,y] = readOn "," . init $ ptS
    [width,height] = readOn "x" $ dimS

    cid = read . tail $ idS
    span = makeRect (Point x y) width height

rectsOverlap :: Rect -> Rect -> Bool
rectsOverlap (Rect a1 b1) (Rect a2 b2) = and $ fs <*> [(a1,b2), (a2,b1)] where
    fs = uncurry <$> [(<=) `on` x, (<=) `on` y]

overlap :: Rect -> Rect -> Maybe Rect
overlap r1@(Rect a1 b1) r2@(Rect a2 b2)
    | rectsOverlap r1 r2 = Just $ Rect (Point xStart yStart) (Point xEnd yEnd)
    | otherwise = Nothing
    where
        endsOn f = if f a2 <= f b1 then (f a2, f b1) else (f a1, f b2)
        (xStart, xEnd) = endsOn x
        (yStart, yEnd) = endsOn y

overlaps :: [Rect] -> Maybe [Rect]
overlaps = undefined
