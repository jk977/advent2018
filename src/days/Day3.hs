module Day3 where

import Data.List (init, nub)
import Util

data Claim = Claim {
    cid :: Int,
    section :: Rect
} deriving (Show, Ord, Eq)

makeRect :: Point -> Int -> Int -> Rect
makeRect start@(Point x y) width height = Rect start end where
    xEnd = x + width - 1    -- -1 to include starting point in width
    yEnd = y + height - 1
    end = Point xEnd yEnd

parseClaim :: String -> Claim
parseClaim s = Claim cid section where
    readOn d = map read . splitOn d

    [idS,_,ptS,dimS] = words s
    [x,y] = readOn "," . init $ ptS
    [width,height] = readOn "x" $ dimS

    cid = read . tail $ idS
    section = makeRect (Point x y) width height

claimsOverlap :: Claim -> Claim -> Bool
claimsOverlap (Claim _ r1) (Claim _ r2) = rectsOverlap r1 r2

-- points that lie in both claims
overlapClaim :: Claim -> Claim -> [Point]
overlapClaim (Claim id1 r1) (Claim id2 r2) = overlapRect r1 r2

-- points that lie in 2 or more of the provided claims
overlaps :: [Claim] -> [Point]
overlaps cs = nub . concat $ pts where
    pts = [overlapClaim c1 c2 | c1 <- cs,
                                c2 <- cs,
                                (cid c1) < (cid c2)]

overlapArea :: [Claim] -> Int
overlapArea = length . overlaps

hasCollisionIn :: Claim -> [Claim] -> Bool
hasCollisionIn c1 = or . map (claimsOverlap c1)

findNonCollisions :: [Claim] -> [Claim]
findNonCollisions = map head
    . filter (\(x:xs) -> not $ hasCollisionIn x xs)
    . rotations

part1 :: IO ()
part1 = getContents
    >>= print
        . overlapArea
        . map parseClaim
        . lines

part2 :: IO ()
part2 = getContents
    >>= putStrLn
        . unlines
        . map (show . cid)
        . findNonCollisions
        . map parseClaim
        . lines
