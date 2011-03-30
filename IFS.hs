module IFS where

type Coord = (Double, Double)

data Geo = Point Coord
         | Line (Coord, Coord)
         | Geos [Geo]
         deriving (Show)

type TransMatrix = (Double, Double, Double, Double, Double, Double)

coordTrans :: TransMatrix -> Coord -> Coord
coordTrans (a, b, c, d, e, f) (x, y) = (a*x + b*y + e, c*x + d*y + f)

{- einzelne Regel -}
geoTrans1 :: TransMatrix -> Geo -> Geo
geoTrans1 table (Point c) = Point $ coordTrans table c
geoTrans1 table (Line (c1, c2)) = let t = coordTrans table in Line (t c1, t c2)
geoTrans1 table (Geos xs) = Geos $ map (geoTrans1 table) xs
-- geoTrans1 _ _ = error "special geoTrans not implemented"

{- mehrere Regeln -}
geoTrans :: [TransMatrix] -> Geo -> Geo
geoTrans tables (Geos xs) = Geos $ concatMap f xs
        where {- rekursive Geos nicht vermeiden: -}
              f (Geos x)  = concatMap (\t -> map (geoTrans1 t) x) tables
              f       x   = map (`geoTrans1` x) tables
geoTrans tables x = Geos $ [geoTrans1 t x | t <- tables]
