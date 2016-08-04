{-# OPTIONS -Wall #-}

module ClipPic where

import Wumpus.Core
import Wumpus.Core.Colour

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out"
    writeEPS "./out/clip_path01.eps" pic1
    writeSVG "./out/clip_path01.svg" pic1





pic1 :: Picture
pic1 = frame [ body ]
  where
    body = clipPrim dog_house $ 
             primGroup [ red_circle, green_circle, blue_circle ]

red_circle :: Primitive 
red_circle = fillEllipse red 60 60      $ P2 (-20) 0

green_circle :: Primitive 
green_circle = fillEllipse green 60 60  $ P2 30 80

blue_circle :: Primitive 
blue_circle = fillEllipse blue 60 60    $ P2 80 0


dog_house :: PrimPath
dog_house = absPrimPath zeroPt $ 
    [ absLineTo  (P2 0 60) 
    , absLineTo  (P2 40 100)
    , absLineTo  (P2 80 60)
    , absLineTo  (P2 80 0)
    , absLineTo  (P2 60 0)  
    , absLineTo  (P2 60 30)
    , absCurveTo (P2 60 50) (P2 50 60) (P2 40 60)
    , absCurveTo (P2 30 60) (P2 20 50) (P2 20 30)
    , absLineTo  (P2 20 0)
    ]


