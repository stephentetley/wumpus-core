{-# OPTIONS -Wall #-}

module Latin1Pic where

import Wumpus.Core
import Wumpus.Core.Colour
import Wumpus.Core.Text.StandardEncoding

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    putStr ps_msg
    writeEPS "./out/latin1_pic01.eps" pic1
    writeSVG "./out/latin1_pic01.svg" pic1
  where
    ps_msg = unlines $ 
        [ "Expected output:"
        , "  SVG - egrave in all four lines."
        , "  GhostScript - Lslash / egrave / Lslash / Lslash." 
        , ""
        , "Explanation:"
        , "The data for this demo uses char code 232 (0xE8) corresponding"
        , "to (egrave) in the common Latin1 character set."
        , "This is printed as expected for SVG."
        , ""
        , "However, in the PostScript _Standard Encoding_ 232 corresponds"
        , "to Lslash and the core GhostScript and Adobe fonts generally use" 
        , "Standard Encoding, so Wumpus matches the font Helvetica with this"
        , "encoding."
        , ""
        , "To get the expected character in both PostScript and SVG use the"
        , "glyph name as the escape token."
        , ""
        ]


-- | Provided the respective lookups can be found, Wumpus 
-- supports escapes as either numbers or names...
--
-- Note - 0xE8 corresponds to Lslash in the standard encoding. 
-- 
pic1 :: Picture
pic1 = frame [ helveticaLabel "myst&#232;re"      (P2 0 60)
             , helveticaLabel "myst&egrave;re"    (P2 0 40) -- no HASH!
             , helveticaLabel "myst&#0o350;re"    (P2 0 20)
             , helveticaLabel "myst&#0XE8;re"     (P2 0 00)
             ]
 



helveticaLabel :: String -> DPoint2 -> Primitive
helveticaLabel ss pt = textlabel black helvetica18 ss pt

helvetica18 :: FontAttr
helvetica18 = FontAttr 18 (FontFace "Helvetica" "Helvetica" 
                                    SVG_REGULAR standard_encoding)
