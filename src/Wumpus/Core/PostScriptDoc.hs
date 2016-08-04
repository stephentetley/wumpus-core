{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PostScriptDoc
-- Copyright   :  (c) Stephen Tetley 2009-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- PostScript Doc combinators.
--
--------------------------------------------------------------------------------

module Wumpus.Core.PostScriptDoc
  ( 
 
    escapeSpecial
  , escapeSpecialChar
    
  , psHeader
  , epsHeader
  , psFooter
  , epsFooter
  , dsc_Page

  , missingCharCode

  , command
  , ps_comment

  , ps_gsave
  , ps_grestore
  , ps_setlinewidth
  , ps_setlinecap
  , ps_setlinejoin
  , ps_setmiterlimit
  , ps_setdash
  , ps_setrgbcolor

  , ps_translate
  , ps_concat

  , ps_newpath
  , ps_moveto
  , ps_rmoveto
  , ps_lineto
  , ps_arc
  , ps_curveto
  , ps_closepath
  , ps_clip

  , ps_fill 
  , ps_stroke
  
  , ps_showpage
  , ps_findfont
  , ps_scalefont
  , ps_setfont
  , ps_show
  , ps_glyphshow

  , ps_wumpus_FELL
  , ps_wumpus_SELL
  , ps_wumpus_FCIRC
  , ps_wumpus_SCIRC
  , ps_wumpus_FL

  , ps_wumpus_prolog

  )  where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Colour
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicProps
import Wumpus.Core.PictureInternal
import Wumpus.Core.Utils.Common
import Wumpus.Core.Utils.FormatCombinators

import Data.Time




--------------------------------------------------------------------------------
-- Escape special chars

-- | Escape these characters:
--
-- > \\ - (, ), <, >, [, ], {, }, /, and %
--
escapeSpecial :: String -> String
escapeSpecial = foldr f "" 
  where
    f c ss | c `elem` ps_special = '\\' : c : ss
           | otherwise           = c : ss

-- Note - this has to promote the Char to a String...
escapeSpecialChar :: Char -> String
escapeSpecialChar c | c `elem` ps_special = ['\\', c]
                    | otherwise           = [c]

ps_special :: [Char]
ps_special = "\\()<>[]{}/%"



psHeader :: Int -> ZonedTime -> Doc
psHeader page_count tod = vcat $ 
    [ text "%!PS-Adobe-3.0"
    , text "%%Pages:"         <+> int page_count
    , text "%%CreationDate:"  <+> parens (showsDoc $ psTimeStamp tod)                
    , text "%%EndComments"
    ]


epsHeader :: DBoundingBox -> ZonedTime -> Doc
epsHeader bb tod = vcat $ 
    [ text "%!PS-Adobe-3.0 EPSF-3.0"
    , text "%%BoundingBox:"   <+> upint llx <+> upint lly
                              <+> upint urx <+> upint ury
    , text "%%CreationDate:"  <+> parens (showsDoc $ psTimeStamp tod)
    , text "%%EndComments"
    ]
  where
    upint             = text . roundup
    (llx,lly,urx,ury) = destBoundingBox bb 



psFooter :: Doc
psFooter = text "%%EOF"


epsFooter :: Doc
epsFooter = vcat [ text "showpage", text "%%EOF" ]


-- | @ %%Page: ... ... @
--
dsc_Page :: String -> Int -> Doc
dsc_Page label ordinal = text "%%Page:" <+> text label <+> int ordinal


missingCharCode :: Int -> String -> Doc
missingCharCode i fallback = vcat $
    [ ps_comment $ "missing lookup for &#" ++ show i ++ ";" 
    , ps_glyphshow fallback
    ]

--------------------------------------------------------------------------------

formatArray :: (a -> Doc) -> [a] -> Doc
formatArray _  [] = brackets space
formatArray fn xs = brackets (separate space $ map fn xs)

command :: String -> [Doc] -> Doc
command cmd [] = text cmd
command cmd ds = hsep ds <+> text cmd 


-- | @ %% ... @
--
ps_comment :: String -> Doc
ps_comment ss = text "%%" <+> text ss

--------------------------------------------------------------------------------
-- Graphics state operators

-- | @ gsave @
--
ps_gsave :: Doc
ps_gsave = command "gsave" []

-- | @ grestore @
--
ps_grestore :: Doc
ps_grestore = command "grestore" []



-- | @ ... setlinewidth @
--
ps_setlinewidth :: Double -> Doc
ps_setlinewidth u = command "setlinewidth" [dtruncFmt u]

-- | @ ... setlinecap @
--
ps_setlinecap :: LineCap -> Doc
ps_setlinecap a = command "setlinecap" [int $ fromEnum a]

-- | @ ... setlinejoin @
--
ps_setlinejoin :: LineJoin -> Doc
ps_setlinejoin a = command "setlinejoin" [int $ fromEnum a]

-- | @ ... setmiterlimit @
--
ps_setmiterlimit :: Double -> Doc
ps_setmiterlimit u = command "setmiterlimit" [dtruncFmt u]

-- | @ [... ...] ... setdash @
--
ps_setdash :: DashPattern -> Doc
ps_setdash Solid          = command "setdash" [text "[]", char '0']
ps_setdash (Dash n pairs) = command "setdash" [brackets $ step pairs, int n]
  where
    step []         = empty
    step [(a,b)]    = int a <+> int b
    step ((a,b):xs) = int a <+> int b <+> step xs  

-- | @ ... ... ... setrgbcolor @
--
ps_setrgbcolor :: RGBi -> Doc
ps_setrgbcolor (RGBi r g b) = command "setrgbcolor" [fn r, fn g, fn b]
  where
    fn i = dtruncFmt $ (fromIntegral i / d255)
    d255 :: Double
    d255 = 255.0

--------------------------------------------------------------------------------
-- coordinate system and matrix operators 

-- | @ ... ... translate @
--
ps_translate :: DVec2 -> Doc
ps_translate (V2 dx dy) = command "translate" [dtruncFmt dx, dtruncFmt dy]


-- Note - Do not use @setmatrix@ for changing the CTM use 
-- @concat@ instead.
--
-- Using concat is harmonious with /nesting/.
-- 

-- | @ [... ... ... ... ... ...] concat @
--
ps_concat :: Matrix3'3 Double -> Doc
ps_concat mtrx = doc <+> text  "concat"
  where 
    (a,b,c,d,e,f) = deconsMatrix mtrx
    doc           = formatArray dtruncFmt [a,b,c,d,e,f]

--------------------------------------------------------------------------------
-- Path construction operators

-- | @ newpath @
--
ps_newpath :: Doc
ps_newpath = command "newpath" []

-- Note - it is apparently preferable to show doubles as 0.0 
-- rather than 0.
--
-- I have read that in PostScript the coercion from int to float
-- is apparently quite expensive (I ought to locate the reference 
-- for this...).

-- | @ ... ... moveto @
--
ps_moveto :: DPoint2 -> Doc
ps_moveto (P2 x y) = command "moveto" [dtruncFmt x, dtruncFmt y]


-- | @ ... ... rmoveto @
--
ps_rmoveto :: DPoint2 -> Doc
ps_rmoveto (P2 x y) = command "rmoveto" [dtruncFmt x, dtruncFmt y]


-- | @ ... ... lineto @
--
ps_lineto :: DPoint2 -> Doc
ps_lineto (P2 x y) = command "lineto" [dtruncFmt x, dtruncFmt y]


-- | @ ... ... ... ... ... arc @
--
ps_arc :: DPoint2 -> Double -> Radian -> Radian -> Doc
ps_arc (P2 x y) radius ang1 ang2 = 
    command "arc" $ [ dtruncFmt x
                    , dtruncFmt y
                    , dtruncFmt radius
                    , dtruncFmt $ fromR ang1
                    , dtruncFmt $ fromR ang2
                    ]
  where
    fromR :: Radian -> Double
    fromR = fromRadian

-- | @ ... ... ... ... ... ... curveto @
--
ps_curveto :: DPoint2 -> DPoint2 -> DPoint2 -> Doc
ps_curveto (P2 x1 y1) (P2 x2 y2) (P2 x3 y3) =
    command "curveto" $ map dtruncFmt [x1,y1, x2,y2, x3,y3]

-- | @ closepath @
--
ps_closepath :: Doc
ps_closepath = command "closepath" []


-- | @ clip @
--
ps_clip :: Doc
ps_clip = command "clip" []


--------------------------------------------------------------------------------
--  painting operators

-- | @ fill @
--
ps_fill :: Doc
ps_fill = command "fill" []

-- | @ stroke @
--
ps_stroke :: Doc
ps_stroke = command "stroke" []

--------------------------------------------------------------------------------
-- Output operators

-- | @ showpage @
ps_showpage :: Doc
ps_showpage = command "showpage" []


--------------------------------------------------------------------------------
-- Character and font operators

-- | The following fonts are expected to exist on most platforms:
--
-- > Times-Roman  Times-Italic  Times-Bold  Times-BoldItalic
-- > Helvetica  Helvetica-Oblique  Helvetica-Bold  Helvetica-Bold-Oblique
-- > Courier  Courier-Oblique  Courier-Bold  Courier-Bold-Oblique
-- > Symbol
--
-- List taken from Bill Casselman \'Mathematical Illustrations\' p279.
--


-- | @ /... findfont @
--
ps_findfont :: String -> Doc
ps_findfont ss = command "findfont" [text $ '/':ss]

-- | @ ... scalefont @
--
ps_scalefont :: Int -> Doc
ps_scalefont sz = command "scalefont" [int sz]

-- | @ setfont @
--
ps_setfont :: Doc
ps_setfont = command "setfont" []

-- | @ (...) show  @
--
ps_show :: String -> Doc
ps_show ss = command "show" [parens $ text ss]


-- | @ (/...) show  @
--
ps_glyphshow :: String -> Doc
ps_glyphshow ss = command "glyphshow" [text $ '/':ss]


--------------------------------------------------------------------------------


-- | @ X Y RX RY FELL  @
--
-- Custom Wumpus proc for filled ellipse.
--
ps_wumpus_FELL :: DPoint2 -> Double -> Double -> Doc
ps_wumpus_FELL (P2 x y) rx ry = 
    command "FELL" $ map dtruncFmt [x, y, rx, ry]



-- | @ X Y RX RY SELL  @
--
-- Custom Wumpus proc for stroked ellipse.
--
ps_wumpus_SELL :: DPoint2 -> Double -> Double -> Doc
ps_wumpus_SELL (P2 x y) rx ry = 
    command "SELL" $ map dtruncFmt [x, y, rx, ry]



-- | @ X Y R FCIRC  @
--
-- Custom Wumpus proc for filled circle.
--
ps_wumpus_FCIRC :: DPoint2 -> Double -> Doc
ps_wumpus_FCIRC (P2 x y) r = command "FCIRC" $ map dtruncFmt [x, y, r]

-- | @ X Y R SCIRC  @
--
-- Custom Wumpus proc for stroked circle.
--
ps_wumpus_SCIRC :: DPoint2 -> Double -> Doc
ps_wumpus_SCIRC (P2 x y) r = command "SCIRC" $ map dtruncFmt [x, y, r]

-- | @ SZ NAME FL  @
--
-- Custom Wumpus proc for findfont, fontsize, setfont.
--
ps_wumpus_FL :: Int -> String -> Doc
ps_wumpus_FL sz name = command "FL" $ [int sz, text $ '/':name]



--------------------------------------------------------------------------------




ps_wumpus_prolog :: Doc
ps_wumpus_prolog = vcat $ map text $
    [ "/RY 0 def"
    , "/RX 0 def"
    , "/Y 0 def"
    , "/X 0 def"
    , "/R 0 def"
    , ""
    , "% Filled ellipse"
    , "/FELL     % X Y RX RY FELL"
    , "{"
    , "  /RY exch def"
    , "  /RX exch def"
    , "  /Y  exch def"
    , "  /X  exch def"
    , "  X Y translate"
    , "  1 RY RX div scale"
    , "  newpath"
    , "  0 0 RX 0.0 360.0 arc"
    , "  closepath"
    , "  fill"
    , "  1 RX RY div scale"
    , "  X neg Y neg translate"
    , "} bind def"
    , ""
    , ""
    , "% Stroked ellipse"
    , "/SELL     % X Y RX RY SELL"
    , "{"
    , "  /RY exch def"
    , "  /RX exch def"
    , "  /Y  exch def"
    , "  /X  exch def"
    , "  X Y translate"
    , "  1 RY RX div scale"
    , "  newpath"
    , "  0 0 RX 0.0 360.0 arc"
    , "  closepath"
    , "  stroke"
    , "  1 RX RY div scale"
    , "  X neg Y neg translate"
    , "} bind def"
    , ""
    , ""
    , "% Stroked circle"
    , "/SCIRC     % X Y R SCIRC"
    , "{"
    , "  /R exch def"
    , "  /Y  exch def"
    , "  /X  exch def"
    , "  newpath"
    , "  X Y R 0.0 360.0 arc"
    , "  closepath"
    , "  stroke"
    , "} bind def"
    , ""
    , ""
    , "% Filled circle"
    , "/FCIRC     % X Y R FCIRC"
    , "{"
    , "  /R exch def"
    , "  /Y  exch def"
    , "  /X  exch def"
    , "  newpath"
    , "  X Y R 0.0 360.0 arc"
    , "  closepath"
    , "  fill"
    , "} bind def"
    , ""
    , ""
    , "% Font load"
    , "/FL   % SZ NAME FL"
    , "{"
    , "  findfont exch"
    , "  scalefont"
    , "  setfont"
    , "} bind def"
    , ""
    ]