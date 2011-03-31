module Main where

{-
Using ImageMagick to convert *.ppm to .gif:
        convert -verbose -delay 50 -loop 0 blatt*.ppm Blatt.gif
-}

import Control.Monad

import IFS.Core
import IFS.Render.PPM


table =
      [
          (0.8492,0.03718,-0.03708,0.8492, 0,1.6),
          (0.1968,-0.2566,0.2264,0.2231, 0,1.6),
          (-0.15,0.2834,0.2598,0.2378,0,0.44),
          (0.0,-0.0,0.0,0.3,0,0.0)
      ]

g0 = ContLines [(-1,0),(-1,1),(1,1),(1,0),(-1,0)]

vectors = iterate (geoTrans table)  g0

main = do
       putStrLn "Breite (in Pixel): "
       width <- getLine >>= return.read
       putStrLn "Height (in Pixel): "
       height <- getLine >>= return.read
       putStrLn "Iterations (> 0): "
       it <- getLine >>= return.read
       forM_ [0 .. it-1] $
             \i -> writePPM ("blatt"++ myshow i ++ ".ppm") $
                       geoPPM' defaultConfig{penColor = (0x51, 0x67, 0x1F)}
                               width height (vectors !! i)
    where myshow i | i < 10 = "00" ++ show i
                   | i < 100 = "0" ++ show i
                   | otherwise = show i
