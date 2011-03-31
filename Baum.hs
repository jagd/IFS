module Main where

{-
Using ImageMagick to convert *.ppm to .gif:
        convert -verbose -delay 50 -loop 0 baum*.ppm Baum.gif
-}

import Control.Monad

import IFS.Core
import IFS.Render.PPM


table =
        [
            ( 0.195, -0.488,  0.344,  0.443, 0.4431, 0.2452),
            ( 0.462,  0.414, -0.252,  0.361, 0.2511, 0.5692),
            (-0.058, -0.07 ,  0.453, -0.111, 0.5976, 0.0969),
            (-0.035,  0.07 , -0.469, -0.022, 0.4884, 0.5069),
            (-0.637,  0.0  ,  0.0  ,  0.501, 0.8562, 0.2513)
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
             \i -> writePPM ("baum"++ myshow i ++ ".ppm") $
                       geoPPM' defaultConfig{penColor = (0x3D, 0x34, 0x49)}
                               width height (vectors !! i)
    where myshow i | i < 10 = "00" ++ show i
                   | i < 100 = "0" ++ show i
                   | otherwise = show i
