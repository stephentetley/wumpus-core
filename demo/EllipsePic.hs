{-# OPTIONS -Wall #-}

module EllipsePic where

import Wumpus.Core
import Wumpus.Core.Colour

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out"
    writeEPS "./out/ellipse01.eps" pic1
    writeSVG "./out/ellipse01.svg" pic1


pic1 :: Picture
pic1 = frame [ ellipse01 $ P2 50 50
             , ellipse02 $ P2 100 50
             , ellipse03 $ P2 150 50
             ]


ellipse01 :: DPoint2 -> Primitive
ellipse01 pt = cstroke black default_stroke_attr $ 
    curvedPrimPath $ bezierEllipse 20 30 pt


ellipse02 :: DPoint2 -> Primitive
ellipse02 pt = cstroke red default_stroke_attr $ 
    curvedPrimPath $ rbezierEllipse 20 30 0 pt



ellipse03 :: DPoint2 -> Primitive
ellipse03 pt = cstroke red default_stroke_attr $ 
    curvedPrimPath $ rbezierEllipse 20 30 (negate $ d2r 10) pt

