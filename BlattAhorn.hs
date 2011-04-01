module Main where

{-
Using ImageMagick to convert *.ppm to .gif:
        convert -verbose -delay 50 -loop 0 ahorn*.ppm Ahorn.gif
-}

import Control.Monad

import IFS.Core
import IFS.Render.PPM


table =
        [
          (0.14,0.01,0.0,0.51,-0.08,-1.31),
          (0.43,0.52,-0.45,0.5,1.49,-0.75),
          (0.45,-0.49,0.47,0.47,-1.62,-0.74),
          (0.49,0.0,0.0,0.51,0.02,1.62)
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
             \i -> writePPM ("ahorn"++ myshow i ++ ".ppm") $
                       geoPPM' defaultConfig{penColor = (0xC6, 0x27, 0x00)}
                               width height (vectors !! i)
    where myshow i | i < 10 = "00" ++ show i
                   | i < 100 = "0" ++ show i
                   | otherwise = show i
