{-# OPTIONS -Wall #-}

module KernPic where

import Wumpus.Core
import Wumpus.Core.Colour
import Wumpus.Core.Text.StandardEncoding
import Wumpus.Core.Text.Symbol
import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    putStrLn $ unlines warn_msg
    writeEPS "./out/kern_pic01.eps" kern_pic
    writeSVG "./out/kern_pic01.svg" kern_pic
  where
    warn_msg = [ "Note - this demo uses the symbol font which is not"
               , "recommended for SVG."
               ]

kern_pic :: Picture
kern_pic = pic1 `picOver` pic2 `picOver` pic3 

pic1 :: Picture
pic1 = frame [ helveticaLabelH universal   (P2 0 50)
             , helveticaLabelH universal   (P2 0 25)
             ]

pic2 :: Picture
pic2 = illustrateBoundsPrim blue_violet $ 
          helveticaLabelV universal (P2 200 180)

pic3 :: Picture
pic3 = frame [ symbolLabelH uUpsilon (P2 0 0) ]


-- Some attention is paid to kerning - note that the kern between 
-- @i@ and @v@ is smaller than the norm.
--
universal ::[KerningChar]
universal = [ kernchar     0  'u'
            , kernchar    15  'n'
            , kernchar    15  'i'
            , kernchar    10  'v'
            , kernchar    15  'e'
            , kernchar    15  'r'
            , kernchar    13  's'
            , kernchar    15  'a'
            , kernchar    15  'l'
            , kernEscName 10  "currency"
            ]

-- Note - this may not work in SVG, some renderers are intolerant 
-- towards the Symbol font.
--
-- 0o241 is upper-case upsilon in the Symbol encoding vector.
-- 
uUpsilon :: [KerningChar]
uUpsilon = [ kernEscInt 6 0o241, kernchar 12 'a', kernchar 12 'b' ] 

helveticaLabelH :: [KerningChar] -> DPoint2 -> Primitive
helveticaLabelH xs pt = hkernlabel black helvetica18 xs pt

helveticaLabelV :: [KerningChar] -> DPoint2 -> Primitive
helveticaLabelV xs pt = vkernlabel black helvetica18 xs pt

symbolLabelH :: [KerningChar] -> DPoint2 -> Primitive
symbolLabelH xs pt = hkernlabel black symbol18 xs pt


helvetica18 :: FontAttr
helvetica18 = FontAttr 18 (FontFace "Helvetica" 
                                    "Helvetica" 
                                    SVG_REGULAR 
                                    standard_encoding)


symbol18 :: FontAttr
symbol18 = FontAttr 18 (FontFace "Symbol" 
                                 "Symbol" 
                                 SVG_REGULAR
                                 symbol_encoding)

blue_violet             :: RGBi
blue_violet             = RGBi 0x8a 0x2b 0xe2
