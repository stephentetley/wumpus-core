{-# OPTIONS -Wall #-}

module LabelPic where

import Wumpus.Core
import Wumpus.Core.Text.StandardEncoding

import System.Directory


main :: IO ()
main = do 
  createDirectoryIfMissing True "./out/"
  sequence_ [ demo01, demo02, demo03, demo04, demo05
            , demo06, demo07 ]



demo01 :: IO ()
demo01 = do 
    writeEPS "./out/label01.eps" lbl1
    writeSVG "./out/label01.svg" lbl1

demo02 :: IO ()
demo02 = do 
    writeEPS "./out/label02.eps" p1
    writeSVG "./out/label02.svg" p1
  where
    p1 = lbl1 `picBeside` lbl1 
              `picBeside` (rotateAbout (pi/4) (center lbl1) lbl1) 
              `picBeside` lbl1

demo03 :: IO ()
demo03 = do 
    writeEPS "./out/label03.eps" p1
    writeSVG "./out/label03.svg" p1
  where
    p1 = (drawBounds lbl1) `picBeside` 
         (drawBounds lbl1) `picBeside` 
         (drawBounds $ rotateAbout (pi/4) (center lbl1) lbl1) `picBeside` 
         (drawBounds lbl1)



demo04 :: IO ()
demo04 = do
    writeEPS "./out/label04.eps" p1
    writeSVG "./out/label04.svg" p1
  where
    p1 =           (drawBounds lbl1) 
         `picOver` (drawBounds $ scale 2 2 lbl1)
         `picOver` (drawBounds $ scale 3 3 lbl1)




bigA, bigB, bigT :: Picture
bigA = bigLetter black 'A'
bigB = bigLetter peru  'B'
bigT = bigLetter plum  'T'

bigLetter :: RGBi -> Char -> Picture
bigLetter rgb ch = scale 5 5 $ frame [textlabel rgb attrs [ch] zeroPt]
  where
    attrs = FontAttr 12 (FontFace "Helvetica" "Helvetica" 
                                  SVG_REGULAR standard_encoding)


-- | A should be above B, above T
demo05 :: IO ()
demo05 = do 
    writeEPS "./out/label05.eps" p1
    writeSVG "./out/label05.svg" p1
  where
    p1 = scale 10 10 $ bigA `picOver` bigB `picOver` bigT



demo06 :: IO ()
demo06 = do 
    writeEPS "./out/label06.eps" p1
    writeSVG "./out/label06.svg" p1
  where
    p1 = pA `picBeside` pB `picBeside` pC `picBeside` pA
    
    pA = drawBounds bigA
    pB = drawBounds $ scale 2 2 bigB
    pC = drawBounds $ picMoveBy `flip` (vec 0 10) $ bigLetter peru 'C'


demo07 :: IO ()
demo07 = do 
    writeEPS "./out/label07.eps" p1
    writeSVG "./out/label07.svg" p1
  where
    p1 = pA `picBeside` pB `picBeside` pC
    
    pA = drawBounds bigA
    pB = drawBounds $ scale 2 2 bigB
    pC = drawBounds $ picMoveBy `flip` (vec 0 10) $ bigLetter peru 'C'




--------------------------------------------------------------------------------



drawBounds :: Picture -> Picture
drawBounds p        = p `picOver` (frame [zcstroke ph])
  where
    ph            = vertexPrimPath $ [bl,br,tr,tl]
    (bl,br,tr,tl) = boundaryCorners $ boundary p


-- | The center of a picture.
center :: Picture -> DPoint2
center a = P2 hcenter vcenter 
  where  
    BBox (P2 x0 y0) (P2 x1 y1) = boundary a
    hcenter                    = x0 + 0.5 * (x1 - x0)
    vcenter                    = y0 + 0.5 * (y1 - y0)

--------------------------------------------------------------------------------

peru :: RGBi
peru = RGBi 205  133  63

plum :: RGBi
plum = RGBi 221  160  221

black :: RGBi
black = RGBi 0 0 0 



lbl1 :: Picture
lbl1 = line1 `picBeside` line2 where
  line1 = frame [textlabel peru attrs "Hello" zeroPt]
  line2 = frame [textlabel peru attrs "World" zeroPt]
  attrs = FontAttr 12 (FontFace "Helvetica" "Helvetica" 
                                SVG_REGULAR standard_encoding)

