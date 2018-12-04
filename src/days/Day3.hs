module Day3 where

import Data.Maybe (catMaybes)
import Data.List (init, nub, intersect)
import Data.Function (on)
import Data.Traversable (sequence)

import Debug.Trace
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
    section :: Rect
} deriving (Show, Ord, Eq)

makeRect :: Point -> Int -> Int -> Rect
makeRect start@(Point x y) width height = Rect start end where
    xEnd = x + width - 1    -- -1 to include starting point in width
    yEnd = y + height - 1
    end = Point xEnd yEnd

pointsIn :: Rect -> [Point]
pointsIn (Rect a b) = [Point x y | x <- [x a..x b], y <- [y a..y b]]

pointsInAll :: [Rect] -> [Point]
pointsInAll = nub . concatMap pointsIn

area :: Rect -> Int
area (Rect a b) = (x b - x a) * (y b - y a)

parseClaim :: String -> Claim
parseClaim s = Claim cid section where
    readOn d = map read . splitOn d

    [idS,_,ptS,dimS] = words s
    [x,y] = readOn "," . init $ ptS
    [width,height] = readOn "x" $ dimS

    cid = read . tail $ idS
    section = makeRect (Point x y) width height

rectsOverlap :: Rect -> Rect -> Bool
rectsOverlap (Rect a1 b1) (Rect a2 b2) = and $ fs <*> [(a1,b2), (a2,b1)] where
    fs = uncurry <$> [(<=) `on` x, (<=) `on` y]

overlapRect :: Rect -> Rect -> [Point]
overlapRect r1@(Rect a1 b1) r2@(Rect a2 b2)
    | rectsOverlap r1 r2 = pointsIn r1 `intersect` pointsIn r2
    | otherwise = []

overlapClaim :: Claim -> Claim -> [Point]
overlapClaim (Claim id1 r1) (Claim id2 r2)
    | id1 /= id2 = overlapRect r1 r2
    | otherwise = []

overlaps :: [Claim] -> [Point]
overlaps cs = nub . concat $ pts where
    pts = [overlapClaim c1 c2 | c1 <- cs, c2 <- cs, (cid c1) < (cid c2)]

overlapArea :: [Claim] -> Int
overlapArea = length . overlaps

part1 :: IO ()
part1 = getContents >>= print . overlapArea . map parseClaim . lines

part2 :: IO ()
part2 = undefined
