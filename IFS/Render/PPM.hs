module IFS.Render.PPM where

import Debug.Trace

{- Binär PPM (P6) -}

import Data.Word
import qualified Data.ByteString as B
import Data.Array
import Data.Array.ST
import Control.Monad
import System.IO

import IFS.Core


colorMax = 255

type Color = (Word8, Word8, Word8)
mkColor :: Integral a => (a,a,a) -> Color
mkColor (r, g, b) = (fromIntegral r, fromIntegral b, fromIntegral g)

data Config = Config {
     pointSize :: Double, -- not implemented
     lineSize  :: Double, -- not implemented
     penColor  :: Color
}

defaultConfig = Config {
        pointSize = 1,
        lineSize = 1,
        penColor = (0,0,0)
}

type PPM = B.ByteString

arrayPPM :: Array (Int,Int) Color -> PPM
arrayPPM arr = B.append ppmHead
                $ B.pack $ concatMap (tripleList.snd) (assocs arr)
        where ((h1,w1),(h2,w2)) = bounds arr
              tripleList (a,b,c) = [a,b,c]
              ppmHead = B.pack $ [0x50, 0x36, 0x0A] {- P6\n -}
                         ++ map (fromIntegral.fromEnum) (show (w2-w1+1) ++ " ")
                         ++ map (fromIntegral.fromEnum) (show (h2-h1+1) ++ "\n")
                         ++ map (fromIntegral.fromEnum) (show colorMax ++ "\n")


writePPM :: String -> PPM -> IO ()
writePPM fn ppm = do
                  h <- openFile fn WriteMode
                  B.hPutStr h ppm
                  hClose h

{- Hilfenfunktionen -}
roundCoord :: Coord -> (Int, Int)
roundCoord (x, y) = (round x, round y)
floorCoord :: Coord -> (Int, Int)
floorCoord (x, y) = (floor x, floor y)

exchange (x,y) = (y,x)

step a s = [a, a+s .. ]
step1 a b | b - a > 0 = [a, a+1 .. b]
          | otherwise = [a, a-1 .. b]

sign x | x > 0  = 1
       | x == 0 = 0
       | otherwise = -1


geoPPM :: Config -> Int -> Geo -> PPM
geoPPM conf w g = arrayPPM $ geoArray conf w g

geoPPM' :: Config -> Int -> Int -> Geo -> PPM
geoPPM' conf w h g = arrayPPM $ geoArray' conf w h g

geoArray' :: Config -> Int -> Int -> Geo -> Array  (Int, Int) Color
geoArray' conf w h geo = runSTArray $
        do
        arr <- newArray ((-1,-1), (h+1,w+1)) (colorMax,colorMax,colorMax)
        paintGeo conf arr geo''
        return arr
    where -- muss optimiert durch verkettung
          geo'  = geoMirrorX geo
          geo'' = (geoScal' xScalRate yScalRate . geoMove (-xmin) (-ymin)) geo'
          ((xmino, ymino), (xmaxo, ymaxo)) = findGeoBound geo'
          (xmin, xmax) = if xmino == xmaxo
                           then (xmino - 1, xmaxo + 1)
                           else (xmino, xmaxo)
          (ymin, ymax) = if ymino == ymaxo
                           then (ymino - 1, ymaxo + 1)
                           else (ymino, ymaxo)
          ow = xmax - xmin -- original width
          oh = ymax - ymin -- original height
          xScalRate = (fromIntegral w) / ow
          yScalRate = (fromIntegral h) / oh

geoArray :: Config -> Int -> Geo -> Array  (Int, Int) Color
geoArray conf w geo = runSTArray $
        do
        arr <- newArray ((-1,-1), (h+1,w+1)) (colorMax,colorMax,colorMax)
        paintGeo conf arr geo''
        return arr
    where -- muss optimiert durch verkettung
          geo'  = geoMirrorX geo
          geo'' = (geoScal scalRate . geoMove (-xmin) (-ymin)) geo'
          ((xmino, ymino), (xmaxo, ymaxo)) = findGeoBound geo'
          (xmin, xmax) = if xmino == xmaxo
                           then (xmino - 1, xmaxo + 1)
                           else (xmino, xmaxo)
          (ymin, ymax) = if ymino == ymaxo
                           then (ymino - 1, ymaxo + 1)
                           else (ymino, ymaxo)
          ow = xmax - xmin -- original width
          oh = ymax - ymin -- original height
          hwRate = oh / ow
          scalRate = (fromIntegral w) / ow
          h = round $ hwRate * (fromIntegral w)


paintGeo conf arr (Point (x,y)) =
        writeArray arr (roundCoord (y,x)) $ penColor conf
{- Bresenham-Algorithmus: -}
paintGeo conf arr (Line (c1, c2)) =
        let points = map exchange $ bresenham c1 c2
        in forM_ points $ \c -> writeArray arr c $ penColor conf
paintGeo conf arr (ContLines (p:ps)) = foldM_ f p ps
    where
      f p1 p2 = do
        let points = map exchange $ bresenham p1 p2
        forM_ points $ \c -> writeArray arr c $ penColor conf
        return p2
paintGeo conf arr (Geos gs) = forM_ gs $ paintGeo conf arr


bresenham (x1,y1) (x2,y2) = map floorCoord $
        if adx > ady
          then zip (step1 x1 x2) (step y1 (dy/abs dx))
          else zip (step x1 (dx/abs dy)) (step1 y1 y2)
    where dx = x2 - x1
          dy = y2 - y1
          adx = abs dx
          ady = abs dy
