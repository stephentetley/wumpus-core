{-# OPTIONS -Wall #-}

-- SVG ouptut has some ability to minimization font change code.
-- This example demonstrates it.

module DeltaPic where

import Wumpus.Core
import Wumpus.Core.Text.StandardEncoding
import System.Directory


peru :: RGBi
peru = RGBi 205  133  63

black :: RGBi
black = RGBi 0 0 0 



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS "./out/delta_pic01.eps" pic1
    writeSVG "./out/delta_pic01.svg" pic1


pic1 :: Picture
pic1 = frame1 $ fontDeltaContext delta_ctx $ primGroup  
          [ helveticaLabel 18 "Optimized - size and face"  (P2 0 60)
          , helveticaLabel 14 "Optimized - face only"      (P2 0 40)
          , courierLabel      "No optimization"            (P2 0 20)
          ]
  where
   delta_ctx  = FontAttr 18 common_ff
   frame1 a   = frame [a] 

common_ff :: FontFace
common_ff = FontFace "Helvetica" "Helvetica" SVG_REGULAR standard_encoding


-- Note - each label is fully attributed with the font style.
-- There really is not attribute inheritance.
--
helveticaLabel :: Int -> String -> DPoint2 -> Primitive
helveticaLabel sz ss pt = textlabel peru attrs ss pt
  where
    attrs = FontAttr sz common_ff

courierLabel :: String -> DPoint2 -> Primitive
courierLabel ss pt = textlabel black wumpus_default_font ss pt