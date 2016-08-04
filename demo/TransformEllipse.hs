{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

module TransformEllipse where

import Wumpus.Core
import Wumpus.Core.Colour

import Data.AffineSpace                         -- package: vector-space

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS "./out/transform_ellipse01.eps" pic1
    writeSVG "./out/transform_ellipse01.svg" pic1
    writeEPS "./out/transform_ellipse02.eps" pic2
    writeSVG "./out/transform_ellipse02.svg" pic2
    writeEPS "./out/transform_ellipse03.eps" pic3
    writeSVG "./out/transform_ellipse03.svg" pic3
    writeEPS "./out/transform_ellipse04.eps" pic4
    writeSVG "./out/transform_ellipse04.svg" pic4
    writeEPS "./out/transform_ellipse05.eps" pic5
    writeSVG "./out/transform_ellipse05.svg" pic5

gray :: RGBi
gray = RGBi 127 127 127

pic1 :: Picture
pic1 = cb `picOver` ell `picOver` xy_frame "no transform"
  where
     ell  = mkRedEllipse id 20 10 pt
     cb   = crossbar 20 10 pt
     pt   = P2 70 10


pic2 :: Picture
pic2 = cb `picOver` ell `picOver` xy_frame "rotate 30deg"
  where
     ell  = mkRedEllipse (rotate ang) 20 10 pt
     cb   = rotate ang $ crossbar 20 10 pt
     pt   = P2 70 10
     ang  = d2r 30

pic3 :: Picture
pic3 = cb `picOver` ell `picOver` xy_frame "rotateAbout (60,0) 30deg"
  where
     ell  = mkRedEllipse (rotateAbout ang pto) 20 10 pt
     cb   = rotateAbout ang pto $ crossbar 20 10 pt
     pt   = P2 70 10
     pto  = P2 60 0 `asTypeOf` dpt
     ang  = d2r 30


pic4 :: Picture
pic4 = cb `picOver` ell `picOver` xy_frame "scale 1 2"
  where
     ell  = mkRedEllipse (scale 1 2) 20 10 pt
     cb   = scale 1 2 $ crossbar 20 10 pt
     pt   = P2 70 10

pic5 :: Picture
pic5 = cb `picOver` ell `picOver` xy_frame "translate -70 -10"
  where
     ell  = mkRedEllipse (translate (-70) (-10)) 20 10 pt
     cb   = translate (-70) (-10) $ crossbar 20 10 pt
     pt   = P2 70 10


mkRedEllipse ::(Primitive -> Primitive) 
             -> Double -> Double -> DPoint2 -> Picture
mkRedEllipse trafo rx ry pt = 
    illustrateControlPoints gray $ trafo $ fillEllipse red rx ry pt

crossbar :: Double -> Double -> DPoint2 -> Picture
crossbar rx ry ctr = 
    frame [ostroke black default_stroke_attr $ absPrimPath west ps]
  where
    ps    = [ absLineTo east, absLineTo ctr, absLineTo north, absLineTo south ]
    north = ctr .+^ vvec ry
    south = ctr .-^ vvec ry 
    east  = ctr .+^ hvec rx
    west  = ctr .-^ hvec rx


xy_frame :: String -> Picture
xy_frame ss = 
    frame [ mkline (P2 (-4) 0) (P2 150 0)
          , mkline (P2 0 (-4)) (P2 0 150) 
          , textlabel black wumpus_default_font ss (P2 0 (-20))
          ]

  where
    mkline p1 p2 = ostroke black default_stroke_attr $ 
                     absPrimPath p1 [absLineTo p2]

dpt :: DPoint2
dpt = zeroPt