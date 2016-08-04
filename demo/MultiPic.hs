{-# OPTIONS -Wall #-}

module MultiPic where

import Wumpus.Core
import Wumpus.Core.Colour

import Data.AffineSpace

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS "./out/multi_pic.eps"  pic1
    writeSVG "./out/multi_pic.svg"  pic1



pic1 :: Picture
pic1 = scale 2 2 $ frame $ 
    [ fillEllipse blue 10 10  zeroPt
    , fillEllipse red 10 10   (P2 40 40)
    , ztextlabel "Wumpus!"    (P2 40 20)
    , square red 5            (P2 50 10)  
    ]


square :: RGBi -> Double -> DPoint2 -> Primitive
square rgb sidelen bl = fill rgb $ vertexPrimPath $
    [bl, bl .+^ hvec sidelen, bl .+^ V2 sidelen sidelen, bl .+^ vvec sidelen]

-- The PostScript generated from this is pretty good.
-- 
-- No extraneous use of @concat@.
--
