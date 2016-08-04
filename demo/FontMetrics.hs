{-# OPTIONS -Wall #-}

module FontMetrics where

import Wumpus.Core
import Wumpus.Core.FontSize
import Wumpus.Core.Text.StandardEncoding

import Data.AffineSpace                 -- package: vector-space

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS "./out/font_metrics.eps" metrics_pic
    writeSVG "./out/font_metrics.svg" metrics_pic


peru :: RGBi
peru = RGBi 205  133  63

plum :: RGBi
plum = RGBi 221  160  221

black :: RGBi
black = RGBi 0 0 0 

courier_attr :: FontAttr
courier_attr = FontAttr 48 (FontFace "Courier" "Courier New" 
                                     SVG_REGULAR standard_encoding)

metrics_pic :: Picture
metrics_pic = char_pic `picOver` lines_pic

lines_pic   :: Picture
lines_pic   = frame $ 
    [ ascender_line, cap_line, xheight_line, baseline, descender_line ]
  where
    ascender_line   = haxis peru (ascenderHeight 48)
    cap_line        = haxis peru (capHeight 48)
    xheight_line    = haxis peru (xcharHeight 48)
    baseline        = haxis peru 0
    descender_line  = haxis peru (descenderDepth 48)



char_pic :: Picture
char_pic = frame $ zipWith ($) chars (iterate (.+^ hvec 32) zeroPt)
  where
    chars = map letter "ABXabdgjxy12"



letter :: Char -> DPoint2 -> Primitive
letter ch pt = textlabel black courier_attr [ch] pt


haxis :: RGBi -> Double -> Primitive
haxis rgb ypos = 
    ostroke rgb dash_attr $ vertexPrimPath [ pt, pt .+^ hvec 440 ]
  where
    dash_attr = default_stroke_attr { dash_pattern = Dash 0 [(2,2)] }
    pt        = P2 0 ypos