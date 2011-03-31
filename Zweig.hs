module Main where

import Control.Monad

import IFS.Core
import IFS.Render.PPM


zweigTable =
        [
            ( -0.467, 0.002, -0.113, 0.015, 0.4, 0.4),
            ( 0.387, 0.43, 0.43, -0.387, 0.256, 0.522),
            ( 0.441, -0.091, -0.009, -0.322, 0.421, 0.505)
        ]

g0 = ContLines [(-1,0),(-1,1),(1,1),(1,0),(-1,0)]

zweigVectors = iterate (geoTrans zweigTable)  g0

main = do
       putStrLn "Breite (in Pixel): "
       width <- getLine >>= return.read
       putStrLn "Height (in Pixel): "
       height <- getLine >>= return.read
       putStrLn "Iterations (> 0): "
       it <- getLine >>= return.read
       forM_ [0 .. it-1] $
             \i -> writePPM ("zweig"++ myshow i ++ ".ppm") $
                       geoPPM' defaultConfig{penColor = (0x0B, 0x7D, 0x0F)}
                               width height (zweigVectors !! i)
    where myshow i | i < 10 = "00" ++ show i
                   | i < 100 = "0" ++ show i
                   | otherwise = show i
