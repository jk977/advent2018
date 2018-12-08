module Day6 where

import Control.Arrow
import Control.Monad

import Data.Function (on)
import Data.List

import Debug.Trace
import Util

data Status = Taken Point | Attempt Point | Tie | Empty
    deriving (Show, Eq)

-- assumes no source point will ever try taking the same point twice
instance Semigroup Status where
    Empty <> s = s
    s <> Empty = s

    Attempt p1 <> Attempt p2
        | p1 == p2 = Attempt p1
        | otherwise = Tie

    Taken p1 <> Taken p2
        | p1 == p2 = Taken p1
        | otherwise = undefined

    Taken p <> _ = Taken p
    _ <> Taken p = Taken p

    Attempt _ <> Tie       = Tie
    Tie <> Attempt _       = Tie
    Tie <> Tie             = Tie

instance Monoid Status where
    mempty = Empty

isTaken :: Status -> Bool
isTaken (Taken _) = True
isTaken _         = False

takens :: [(Point, Status)] -> [(Point, Status)]
takens = filter (isTaken . snd)

enclose :: [Point] -> Rect
enclose ps = Rect (Point xMin yMin) (Point xMax yMax) where
    extremaOn f = [minimum, maximum] <*> [f <$> ps]
    [xMin, xMax] = extremaOn x
    [yMin, yMax] = extremaOn y

makeGrid :: [Point] -> [(Point, Status)]
makeGrid ps = replace <$> blankGrid where
    region = pointsIn $ enclose ps
    blankGrid = zip region (repeat Empty)
    replace cell@(p, _) = if p `elem` ps then (p, Taken p) else cell

adjacents :: Point -> [(Point, Status)] -> [(Point, Status)]
adjacents p grid = nub $ do
    potential <- [addPt, subPt] <*> [Point 0 1, Point 1 0] <*> [p]
    filter ((==potential) . fst) grid

finalize :: Status -> Status
finalize (Attempt source) = Taken source
finalize s                = s

step :: [(Point, Status)] -> [(Point, Status)]
step grid = map (second finalize) . combineOn (<>) mempty $ do
    (p, stat)   <- takens grid
    (p', stat') <- adjacents p grid
    update p' (const $ Attempt p <> stat') grid

hasEmpty :: [(Point, Status)] -> Bool
hasEmpty = or . map ((==Empty) . snd)

fillGrid :: [(Point, Status)] -> [(Point, Status)]
fillGrid = until (not . hasEmpty) (\g -> trace (show g ++ "\n") (step g))

showGrid :: [(Point, Status)] -> IO ()
showGrid grid = forM_ (sortOn  $ 

borderPoints :: [Point] -> [Point]
borderPoints ps = nub $ concat [xExtrema, yExtrema] where
    bordersOn f = [minimumBy, maximumBy] <*> pure (compare `on` f) <*> pure ps
    xExtrema = bordersOn x
    yExtrema = bordersOn y

area :: [(Point, Status)] -> Int
area grid = length $ filter (isFinite . snd) filled where
    borders = borderPoints $ fst <$> grid
    filled = filter (isTaken . snd) grid
    isFinite (Taken p) = not $ p `elem` borders

readPoint :: String -> Point
readPoint s = Point x y where
    [(x':_), y'] = words s
    (x,y) = (read [x'], read y')

part1 :: IO ()
part1 = do
    grid <- makeGrid . map readPoint . lines <$> getContents
    let filled = fillGrid grid

    print . area . fillGrid $ grid

part2 :: IO ()
part2 = undefined
