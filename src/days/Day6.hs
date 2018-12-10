module Day6 where

import Control.Monad (forM_)

import Data.Function (on)
import Data.List     (sortOn, nub)
import Data.Maybe    (catMaybes)

import GHC.Exts (groupWith)
import Util     (Point(..), subPt, enclose, pointsIn)

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

dirComponents :: FlatRay -> (Int,Int)
dirComponents (FlatRay Horiz d p)
    | d == Positive = (1,0)
    | otherwise     = (-1,0)
dirComponents (FlatRay Vert d p)
    | d == Positive = (0,1)
    | otherwise     = (0,-1)

intersection :: FlatRay -> FlatRay -> Maybe Point
intersection r1@(FlatRay rt1 d1 p1) r2@(FlatRay rt2 d2 p2) = do
    let (dx1,dy1) = dirComponents r1
        (dx2,dy2) = dirComponents r2
        diffX = x p2 - x p1
        diffY = y p2 - y p1
        det = dx2 * dy1 - dy2 * dx1

        d1 = (diffY * dx2 - diffX * dy2) `div` det
        d2 = (diffY * dx1 - diffX * dy1) `div` det

    if (det /= 0) && (d1 >= 0) && (d2 >= 0) then
        let x' = x p1 + d1 * dx1
            y' = y p1 + d1 * dy1
        in  return $ Point x' y'
    else
        Nothing

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

-- this is used four times (once for each cardinal direction) for each source
-- point to detect if the area occupied by the source point is finite
isLimitedBy :: FlatRay -> Point -> Bool
isLimitedBy ray p = case distanceFrom ray p of
    Just dist   -> dist <= abs (flatDistanceFrom ray p)
    _           -> False

raysFrom :: Point -> [FlatRay]
raysFrom p = do
    rt  <- [Horiz, Vert]
    dir <- [Positive, Negative]
    return $ FlatRay rt dir p

hasFiniteAreaIn :: [Point] -> Point -> Bool
hasFiniteAreaIn sources p = and $ do
    let others  = filter (/=p) sources
    ray         <- raysFrom p
    return . or $ (ray `isLimitedBy`) <$> others

-- gets rough bounds of area taken by point to reduce number of calculations
boundsOf :: Point -> [Point] -> [Point]
boundsOf p sources = nub . catMaybes $ do
    source <- filter (/=p) sources
    r1     <- raysFrom p

    if r1 `isLimitedBy` source then do
        r2 <- raysFrom source
        return $ intersection r1 r2
    else
        return Nothing

manhattanDistance :: Point -> Point -> Int
manhattanDistance p1 p2 = abs x + abs y where
    Point x y = subPt p1 p2

closestTo :: [Point] -> [Point] -> [(Point, Maybe Point)]
closestTo ps sources = do
    p <- ps

    let others = filter (/=p) sources
        labeled = zip sources $ manhattanDistance p <$> sources
        options = map fst . head $ groupWith snd labeled

    if length options /= 1 then
        return (p, Nothing)
    else
        return (p, Just $ head options)

areaTakenBy :: Point -> [Point] -> Int
areaTakenBy p sources = length . filter (maybe False (==p) . snd) $ sourceMap where
    bounds = boundsOf p sources
    potentials = pointsIn $ enclose bounds
    sourceMap = closestTo potentials sources

readPoint :: String -> Point
readPoint s = Point x y where
    [(x':_), y'] = words s
    (x,y) = (read [x'], read y')

part1 :: IO ()
part1 = do
    seeds <- map readPoint . lines <$> getContents

    let finites = filter (hasFiniteAreaIn seeds) seeds
        areas  = length . flip boundsOf seeds <$> finites

    undefined

part2 :: IO ()
part2 = undefined
