module PPM where

{- Binär PPM (P6) -}

import Data.Word
import qualified Data.ByteString as B
import Data.Array
import System.IO

type PPM = B.ByteString

byteToPPM :: Array (Int,Int) (Word8, Word8, Word8) -> PPM
byteToPPM arr = B.append ppmHead
                $ B.pack $ concatMap (tripleList.snd) (assocs arr)
        where ((h1,w1),(h2,w2)) = bounds arr
              tripleList (a,b,c) = [a,b,c]
              ppmHead = B.pack $ [0x50, 0x36, 0x0A] {- P6\n -}
                         ++ map (fromIntegral.fromEnum) (show (w2-w1+1) ++ " ")
                         ++ map (fromIntegral.fromEnum) (show (h2-h1+1) ++ "\n")
                         ++ map (fromIntegral.fromEnum) "255\n"


writePPM :: String -> PPM -> IO ()
writePPM fn ppm = do
                  h <- openFile fn WriteMode
                  B.hPutStr h ppm
                  hClose h

testArray :: Int -> Int -> Array (Int,Int) (Word8, Word8, Word8)
testArray w h = array ((1,1),(h,w))
                      [((i,j), (fromIntegral (i*255`div`h),
                               fromIntegral (i*255`div`h),
                               fromIntegral (i*j `mod` 255)))
                               | i <- [1..h], j<-[1..w]]

testPPM w h= byteToPPM $ testArray w h
