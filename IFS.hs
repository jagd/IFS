module IFS where

type Coord = (Double, Double)

data Geo = Point Coord
         | Line (Coord, Coord)
         | ContLines [Coord] -- Mehr als 2 Punkten !
         | Geos [Geo]
         deriving (Show)

type TransMatrix = (Double, Double, Double, Double, Double, Double)

coordTrans :: TransMatrix -> Coord -> Coord
coordTrans (a, b, c, d, e, f) (x, y) = (a*x + b*y + e, c*x + d*y + f)


{- einzelne Regel -}
geoTrans1 :: TransMatrix -> Geo -> Geo
geoTrans1 table (Point c) = Point $ coordTrans table c
geoTrans1 table (Line (c1, c2)) = let t = coordTrans table in Line (t c1, t c2)
geoTrans1 table (ContLines cs) = ContLines $ map (coordTrans table) cs
geoTrans1 table (Geos xs) = Geos $ map (geoTrans1 table) xs
-- geoTrans1 _ _ = error "special geoTrans not implemented"


{- mehrere Regeln -}
geoTrans :: [TransMatrix] -> Geo -> Geo
geoTrans tables (Geos xs) = Geos $ concatMap f xs
        where {- rekursive Geos vermeiden: -}
              f (Geos x)  = concatMap (\t -> map (geoTrans1 t) x) tables
              f       x   = map (`geoTrans1` x) tables
geoTrans tables x = Geos $ [geoTrans1 t x | t <- tables]


{- Den kleinste Rechteck zu finden,
   in dem die ganze Struktur enthält werden kann -}
findGeoBound :: Geo -> (Coord, Coord) {- Ausgabe: (Linksuntern, Rechtsoben) -}
findGeoBound (Point c) = (c, c)
findGeoBound (Line ((x1,y1), (x2,y2)))
                     | x1 <= x2  = if y1 <= y2
                                     then ((x1, y1), (x2, y2))
                                     else ((x1,y2), (x2,y1))
                     | y1 <= y2  = ((x2, y1), (x1,y2))
                     | otherwise = ((x2,y2), (x1,y1))
findGeoBound (ContLines cs) = let (xs, ys) = unzip cs
                              in ((minimum xs, minimum ys),
                                  (maximum xs, maximum ys))
findGeoBound (Geos gs) = let (c1, c2) = unzip $ map findGeoBound gs
                             (x1, y1) = unzip c1
                             (x2, y2) = unzip c2
                         in ((minimum x1, minimum y1), (maximum x2, maximum y2))


geoMove x y = geoTrans1 (1,0,0,1, x,y)
geoRot phi = geoTrans1 (c,-s,s,c, 0,0)
        where c = cos phi
              s = sin phi
geoScal' x y = geoTrans1 (x,0,0,y, 0,0)
geoScal  m   = geoTrans1 (m,0,0,m, 0,0)
geoShearX m = geoTrans1 (1,m,0,1, 0,0)
geoShearY m = geoTrans1 (1,0,m,1, 0,0)
geoMirrorX = geoTrans1 (1,0,0,-1, 0,0)
geoMirrorY = geoTrans1 (-1,0,0,1, 0,0)
geoMirrorPhi phi = geoRot (-phi) . geoMirrorX . geoRot phi
