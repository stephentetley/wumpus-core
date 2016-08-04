{-# OPTIONS -Wall #-}

module WorldFrame where

import Wumpus.Core
import Wumpus.Core.Colour ( black )
import Wumpus.Core.Text.StandardEncoding

main :: IO ()
main = writeEPS "./out/WorldFrame.eps" world_frame >>
       writeSVG "./out/WorldFrame.svg" world_frame

world_frame :: Picture
world_frame = scale 0.75 0.75 $ 
    frame [ ogin, btm_right, top_left, top_right
          , x_axis, y_axis, line1
          ]
  where
    ogin      = makeLabelPrim "(0,0)"     (P2 0  0)
    btm_right = makeLabelPrim "(100,0)"   (P2 96 0)
    top_left  = makeLabelPrim "(0,100)"   (P2 0  114)
    top_right = makeLabelPrim "(100,100)" (P2 96 114)
    
    x_axis    = makeLinePrim 0.5 (P2 10 10) (P2 110 10)
    y_axis    = makeLinePrim 0.5 (P2 10 10) (P2 10 110)
    line1     = makeLinePrim 1.5 (P2 11 11) (P2 110 110)



makeLabelPrim :: String -> DPoint2 -> Primitive
makeLabelPrim = textlabel black attrs 
  where
    attrs     = FontAttr 10 (FontFace "Helvetica" "Helvetica" 
                                      SVG_REGULAR standard_encoding)

makeLinePrim :: Double -> DPoint2 -> DPoint2 -> Primitive
makeLinePrim lw a b = ostroke black attrs $ absPrimPath a [absLineTo b]
  where
    attrs = default_stroke_attr { line_width =  lw }

