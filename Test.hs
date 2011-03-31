module Test where

import IFS
import Render.PPM


import Data.Array
import Data.Word

{- IFS -}

tableZweig =
        [
            ( -0.467, 0.002, -0.113, 0.015, 0.4, 0.4),
            ( 0.387, 0.43, 0.43, -0.387, 0.256, 0.522),
            ( 0.441, -0.091, -0.009, -0.322, 0.421, 0.505)
        ]

testGeos = geoTrans tableZweig $ Geos [Point (0,0), Geos [Point(0,0), Line ((1,1), (2,2))]]

testFindGeoBound =  findGeoBound (Geos [Point (0,0), Point (0,2),
                                  Line ((1,0.5), (-1,1.5)),
                                  ContLines [(0,0), (1,1), (0,2), (-1,2)]
                                  ]) == ((-1,0), (1,2))

{- PPM -}
testArray :: Int -> Int -> Array (Int,Int) (Word8, Word8, Word8)
testArray w h = array ((1,1),(h,w))
                      [((i,j), (fromIntegral (i*255`div`h),
                               fromIntegral (i*255`div`h),
                               fromIntegral (i*j `mod` 255)))
                               | i <- [1..h], j<-[1..w]]

testPPM w h= byteToPPM $ testArray w h
writeTestPPM = writePPM "test.ppm" $ testPPM 600 480
