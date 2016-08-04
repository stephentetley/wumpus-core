{-# OPTIONS -Wall #-}

module TransformTextlabel where

import Wumpus.Core
import Wumpus.Core.Colour

import Data.AffineSpace                         -- package: vector-space

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS "./out/transform_textlabel01.eps" pic1
    writeSVG "./out/transform_textlabel01.svg" pic1
    writeEPS "./out/transform_textlabel02.eps" pic2
    writeSVG "./out/transform_textlabel02.svg" pic2
    writeEPS "./out/transform_textlabel03.eps" pic3
    writeSVG "./out/transform_textlabel03.svg" pic3
    writeEPS "./out/transform_textlabel04.eps" pic4
    writeSVG "./out/transform_textlabel04.svg" pic4
    writeEPS "./out/transform_textlabel05.eps" pic5
    writeSVG "./out/transform_textlabel05.svg" pic5



pic1 :: Picture
pic1 = txt `picOver` ch `picOver` xy_frame "no transform"
  where
    txt  = mkBlackTextlabel id pt
    ch   = zcrosshair pt
    pt   = P2 70 10


pic2 :: Picture
pic2 = txt `picOver` ch `picOver` xy_frame "rotate 30deg"
  where
    txt  = mkBlackTextlabel (rotate ang) pt
    ch   = rotate ang $ zcrosshair pt
    pt   = P2 70 10
    ang  = d2r 30


pic3 :: Picture
pic3 = txt `picOver` ch `picOver` xy_frame "rotateAbout (60,0) 30deg"
  where
    txt  = mkBlackTextlabel (rotateAbout ang pto) pt
    ch   = rotateAbout ang pto $ zcrosshair pt
    pt   = P2 70 10
    pto  = P2 60 0 `asTypeOf` dpt
    ang  = d2r 30


pic4 :: Picture
pic4 = txt `picOver` ch `picOver` xy_frame "scale 1 2"
  where
    txt  = mkBlackTextlabel (scale 1 2) pt
    ch   = scale 1 2 $ zcrosshair pt
    pt   = P2 70 10

pic5 :: Picture
pic5 = txt `picOver` ch `picOver` xy_frame "translate -70 -10"
  where
    txt  = mkBlackTextlabel (translate (-70) (-10)) pt
    ch   = translate (-70) (-10) $ zcrosshair pt
    pt   = P2 70 10


mkBlackTextlabel :: (Primitive -> Primitive) -> DPoint2 -> Picture
mkBlackTextlabel trafo bl = 
    frame [ trafo $ textlabel black wumpus_default_font "rhubarb" bl ] 

zcrosshair :: DPoint2 -> Picture
zcrosshair = crosshair 56 12 

crosshair :: Double -> Double -> DPoint2 -> Picture
crosshair w h bl = 
    frame [ostroke burlywood default_stroke_attr $ absPrimPath bl ps]
  where
    ps    = [ absLineTo tr, absLineTo br, absLineTo tl, absLineTo bl ]
    tl    = bl .+^ vvec h
    tr    = bl .+^ vec  w h
    br    = bl .+^ hvec w
    
burlywood :: RGBi
burlywood = RGBi 222 184 135

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