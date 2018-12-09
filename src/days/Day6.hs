module Day6 where

import Data.Function (on)
import Data.List     (sortOn)
import Debug.Trace   (trace)
import Util          (Point(..), subPt)

data FlatRayType = Horiz | Vert
    deriving (Show, Eq)

data Direction = Positive | Negative
    deriving (Show, Eq)

-- represents a ray along an axis
data FlatRay = FlatRay {
    rayType   :: FlatRayType,
    direction :: Direction,
    start     :: Point
} deriving (Show, Eq)

-- gets parallel distance from point to start of ray
flatDistanceFrom :: FlatRay -> Point -> Int
flatDistanceFrom (FlatRay rt _ p') p
    | rt == Horiz = x p - x p'
    | otherwise   = y p - y p'

-- closest distance to ray from point
-- if there isn't a line perpendicular to ray through point, returns Nothing
distanceFrom :: FlatRay -> Point -> Maybe Int
distanceFrom ray@(FlatRay rt d p') p
    | hasDistance = Just distance
    | otherwise = Nothing
    where
        coord = case rt of
            Horiz -> y
            Vert  -> x

        distance    = abs . coord $ subPt p' p
        flatDist    = flatDistanceFrom ray p
        hasDistance =
               (d == Positive) && (flatDist >= 0)
            || (d == Negative) && (flatDist <= 0)

-- checks if ray is limited by point.
-- this is used four times (once for each cardinal direction) for each source
-- point to detect if the area occupied by the source point is finite
isLimitedBy :: FlatRay -> Point -> Bool
isLimitedBy ray p = case distanceFrom ray p of
    Just dist   -> dist <= abs (flatDistanceFrom ray p)
    _           -> False

hasFiniteAreaIn :: [Point] -> Point -> Bool
hasFiniteAreaIn sources p = and $ do
    rt  <- [Horiz, Vert]
    dir <- [Positive, Negative]

    let ray      = FlatRay rt dir p
        others   = filter (/=p) sources
        isFinite = or $ (ray `isLimitedBy`) <$> others

    return isFinite

areaTakenBy :: Point -> [Point] -> Int
areaTakenBy p ps = undefined

manhattanDistance :: Point -> Point -> Int
manhattanDistance p1 p2 = abs x + abs y where
    Point x y = subPt p1 p2

readPoint :: String -> Point
readPoint s = Point x y where
    [(x':_), y'] = words s
    (x,y) = (read [x'], read y')

part1 :: IO ()
part1 = do
    seeds <- map readPoint . lines <$> getContents

    let finites = filter (hasFiniteAreaIn seeds) seeds
    putStrLn . unlines . map show $ finites

part2 :: IO ()
part2 = undefined
