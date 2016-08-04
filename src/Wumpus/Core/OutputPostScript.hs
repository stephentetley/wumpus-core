{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PostScript
-- Copyright   :  (c) Stephen Tetley 2009-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output PostScript - either PostScript (PS) files or 
-- EPS (Encapusulated PostScript) files can be generated. 
-- 
-- The generated PostScript uses only features from Language 
-- Level 1.
--
--------------------------------------------------------------------------------

module Wumpus.Core.OutputPostScript
  ( 
  -- * Output PostScript
    writePS
  , writeEPS
 
  ) where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Colour
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicProps
import Wumpus.Core.PictureInternal
import Wumpus.Core.PostScriptDoc
import Wumpus.Core.Text.Base
import Wumpus.Core.Text.GlyphNames
import Wumpus.Core.TrafoInternal
import Wumpus.Core.Utils.JoinList hiding ( cons )
import Wumpus.Core.Utils.FormatCombinators

import Data.AffineSpace                         -- package: vector-space

import Control.Monad

import Data.Char
import qualified Data.Foldable          as F
import qualified Data.IntMap            as IntMap
import Data.List ( mapAccumL )
import Data.Maybe 
import Data.Time


--------------------------------------------------------------------------------
-- PsMonad

-- PsMonad is at least a Reader and a State...
--
-- Graphics state works differently to SVG - PostScript has no 
-- nesting (at least not in the code Wumpus generates) so there
-- are no /savings/ by diffing from an outer environment, instead
-- the diff is with the last drawn object.
--

newtype PsMonad a = PsMonad { 
            getPsMonad :: GraphicsState -> (a,GraphicsState) }


instance Functor PsMonad where
  fmap f mf = PsMonad $ \s -> let (a,s1) = getPsMonad mf s in (f a,s1)

instance Applicative PsMonad where
  pure a    = PsMonad $ \s -> (a,s)
  mf <*> ma = PsMonad $ \s -> let (f,s1) = getPsMonad mf s
                                  (a,s2) = getPsMonad ma s1
                                in (f a,s2)

instance Monad PsMonad where
  return a  = PsMonad $ \s -> (a,s)
  m >>= k   = PsMonad $ \s -> let (a,s1) = getPsMonad m s
                              in (getPsMonad . k) a s1
                              

runPsMonad :: PsMonad a -> a
runPsMonad mf = fst $ getPsMonad mf zeroGS




-- runLocalGS :: GSUpdate -> PsMonad a -> PsMonad a
-- runLocalGS upd mf = 
--     PsMonad $ \s -> let (a,_) = getPsMonad mf (getGSU upd s) in (a,s)



getsGS :: (GraphicsState -> a) -> PsMonad a
getsGS fn = PsMonad $ \s -> (fn s,s)

setsGS :: (GraphicsState -> GraphicsState) -> PsMonad ()
setsGS fn = PsMonad $ \s -> ((),fn s)

resetGS :: PsMonad ()
resetGS = PsMonad $ \_ -> ((),zeroGS)


setsSA :: (StrokeAttr -> StrokeAttr) -> PsMonad ()
setsSA fn = getsGS gs_stroke_attr >>= \sa -> 
            setsGS (\s -> s { gs_stroke_attr = fn sa })

getDrawColour       :: PsMonad RGBi
getDrawColour       = getsGS gs_draw_colour

setDrawColour       :: RGBi -> PsMonad ()
setDrawColour a     = setsGS (\s -> s { gs_draw_colour=a})


getFontAttr         :: PsMonad FontAttr
getFontAttr         = getsGS (\s -> FontAttr (gs_font_size s) (gs_font_face s))

setFontAttr         :: FontAttr -> PsMonad ()
setFontAttr (FontAttr sz ff) = 
    setsGS (\s -> s { gs_font_size=sz, gs_font_face=ff })

  
getLineWidth        :: PsMonad Double
getLineWidth        = getsGS (line_width . gs_stroke_attr)

setLineWidth        :: Double -> PsMonad ()
setLineWidth a      = setsSA (\s -> s { line_width = a } )

getMiterLimit       :: PsMonad Double
getMiterLimit       = getsGS (miter_limit . gs_stroke_attr)

setMiterLimit       :: Double -> PsMonad ()
setMiterLimit a     = setsSA (\s -> s { miter_limit = a } )


getLineCap          :: PsMonad LineCap
getLineCap          = getsGS (line_cap . gs_stroke_attr)

setLineCap          :: LineCap -> PsMonad ()
setLineCap a        = setsSA (\s -> s { line_cap = a })


getLineJoin         :: PsMonad LineJoin
getLineJoin         = getsGS (line_join . gs_stroke_attr)

setLineJoin         :: LineJoin -> PsMonad ()
setLineJoin a       = setsSA (\s -> s { line_join = a })


getDashPattern      :: PsMonad DashPattern
getDashPattern      = getsGS (dash_pattern . gs_stroke_attr)

setDashPattern      :: DashPattern -> PsMonad ()
setDashPattern a    = setsSA (\s -> s { dash_pattern = a })


--------------------------------------------------------------------------------
-- Render to PostScript

-- | Output a series of pictures to a Postscript file. Each 
-- picture will be printed on a separate page. 
--
writePS :: FilePath -> [Picture] -> IO ()
writePS filepath pics = 
    getZonedTime >>= \ztim -> writeFile filepath (show $ psDraw ztim pics)

-- | Output a picture to an EPS (Encapsulated PostScript) file. 
-- The .eps file can then be imported or embedded in another 
-- document.
--
writeEPS :: FilePath -> Picture -> IO ()
writeEPS filepath pic =
    getZonedTime >>= \ztim -> writeFile filepath (show $ epsDraw ztim pic)






--------------------------------------------------------------------------------
-- Internals


-- | Draw a picture, generating PostScript output.
--
-- Note - the bounding box may /below the origin/ - if it is, it
-- will need translating.
--

psDraw :: ZonedTime -> [Picture] -> Doc
psDraw timestamp pics = 
    let body = vcat $ runPsMonad $ zipWithM psDrawPage pages pics
    in vcat [ psHeader (length pics) timestamp
            , ps_wumpus_prolog
            , body
            , psFooter 
            ]
  where
    pages = map (\i -> (show i,i)) [1..]

-- | Note the bounding box may /below the origin/ - if it is, it
-- will need translating.
--
psDrawPage :: (String,Int) -> Picture -> PsMonad Doc
psDrawPage (lbl,ordinal) pic = 
    (\doc -> vcat [ dsc_Page lbl ordinal
                  , ps_gsave
                  , cmdtrans
                  , doc
                  , ps_grestore
                  , ps_showpage
                  ]) 
      <$> picture pic
  where
    (_,cmdtrans) = imageTranslation pic 

-- | Note the bounding box may /below the origin/ - if it is, it
-- will need translating.
--
epsDraw :: ZonedTime -> Picture -> Doc
epsDraw timestamp pic =
    vcat [ epsHeader bb timestamp
         , ps_wumpus_prolog
         , ps_gsave
         , cmdtrans
         , body
         , ps_grestore
         , epsFooter
         ]
  where
    (bb,cmdtrans) = imageTranslation pic 
    body          = runPsMonad (picture pic) 


imageTranslation :: Picture -> (DBoundingBox, Doc)
imageTranslation pic = case repositionDeltas pic of
  (bb, Nothing) -> (bb, empty)
  (bb, Just v)  -> (bb, ps_translate v)

--------------------------------------------------------------------------------
-- 



findGlyphName :: Int -> EncodingVector -> Either String String
findGlyphName i ev = 
    case IntMap.lookup i ev of
      Just n  -> Right n
      Nothing -> Left $ fromMaybe "space" $ IntMap.lookup i ps_glyph_names


psText :: EncodingVector -> EscapedText -> Doc
psText ev enc_text = cons $ destrEscapedText (foldr fn ([],empty)) enc_text
  where
    cons ([],doc)               = doc
    cons (cs,doc)               = ps_show cs `vconcat` doc

    fn (CharLiteral c) (cs,doc) | ord c < 0x80  = (c:cs,doc)
    fn (CharLiteral c) acc      = ([], psSpecial ev (ord c) `vconcat` cons acc) 
    fn (CharEscInt i)  acc      = ([], psSpecial ev i `vconcat` cons acc)
    fn (CharEscName s) acc      = ([], ps_glyphshow s `vconcat` cons acc)  

psChar :: EncodingVector -> EscapedChar -> Doc
psChar _  (CharLiteral c) | ord c < 0x80  = ps_show [c]
psChar ev (CharLiteral c)                 = psSpecial ev (ord c) 
psChar ev (CharEscInt i)                  = psSpecial ev i
psChar _  (CharEscName s)                 = ps_glyphshow s  


psSpecial :: EncodingVector -> Int -> Doc
psSpecial ev i = case findGlyphName i ev of
    Left fallback  -> missingComment i `vconcat` ps_glyphshow fallback
    Right ss       -> ps_glyphshow ss     


missingComment :: Int -> Doc
missingComment i = 
    ps_comment $ "lookup in encoding vector failed for " ++ show i


--------------------------------------------------------------------------------

picture :: Picture -> PsMonad Doc
picture (Leaf    (_,xs) ones)   = bracketTrafos xs $ oneConcat primitive ones
picture (Picture (_,xs) ones)   = bracketTrafos xs $ oneConcat picture ones



oneConcat :: (a -> PsMonad Doc) -> JoinList a -> PsMonad Doc
oneConcat fn ones = outstep (viewl ones)
  where
    outstep (e :< rest)   = fn e >>= \a -> instep a (viewl rest)
    outstep (OneL e)      = fn e
    
    instep ac (OneL e)    = fn e >>= \a -> return (ac `vconcat` a)
    instep ac (e :< rest) = fn e >>= \a -> instep (ac `vconcat` a) (viewl rest)


-- No action is taken for hyperlinks or font context changes in 
-- PostScript.
--
-- PostScript ignores any FontCtx changes via the @Group@ 
-- constructor.
--
-- Also - because Clip uses gsave grestore it has to resetGS on
-- ending, otherwise the next picture will be diffing against a 
-- modified state (in Wumpus land) that contradicts the PostScript 
-- state. 
--

primitive :: Primitive -> PsMonad Doc
primitive (PPath props pp)     
    | isEmptyPath pp           = pure empty 
    | otherwise                = primPath props pp

primitive (PLabel props lbl)   
    | isEmptyLabel lbl         = pure empty
    | otherwise                = primLabel props lbl

primitive (PEllipse props ell) = primEllipse props ell

primitive (PContext _ chi)     = primitive chi

primitive (PSVG _ chi)         = primitive chi

primitive (PGroup ones)        = oneConcat primitive ones

primitive (PClip cp chi) = 
    (\d1 d2 -> vcat [ps_gsave,d1,d2,ps_grestore])
      <$> clipPath cp <*> primitive chi <* resetGS


primPath :: PathProps -> PrimPath -> PsMonad Doc
primPath (CFill rgb)     p = 
    (\rgbd -> vcat [rgbd, pathBody p, ps_closepath, ps_fill]) 
      <$> deltaDrawColour rgb  

primPath (CStroke attrs rgb) p = 
    (\rgbd attrd -> vcat [ rgbd, attrd, pathBody p
                         , ps_closepath, ps_stroke ])
      <$> deltaDrawColour rgb <*> deltaStrokeAttrs attrs
 
primPath (OStroke attrs rgb) p = 
    (\rgbd attrd -> vcat [rgbd, attrd, pathBody p, ps_stroke]) 
      <$> deltaDrawColour rgb <*> deltaStrokeAttrs attrs

primPath (CFillStroke fc attrs sc) p = 
    (\d1 d2 -> vcat [d1,d2])
      <$> primPath (CFill fc) p <*> primPath (CStroke attrs sc) p


clipPath :: PrimPath -> PsMonad Doc
clipPath p = pure $ vcat [pathBody p , ps_closepath, ps_clip]


pathBody :: PrimPath -> Doc
pathBody ppath =
    let (start,xs) = extractRelPath ppath 
    in vcat $ ps_newpath : ps_moveto start : (snd $ mapAccumL step start xs)
  where
    step pt (RelLineTo v)         = let p1 = pt .+^ v in (p1, ps_lineto p1)
    step pt (RelCurveTo v1 v2 v3) = let p1 = pt .+^ v1 
                                        p2 = p1 .+^ v2
                                        p3 = p2 .+^ v3
                                    in (p3, ps_curveto p1 p2 p3)




-- | Drawing stroked ellipse has an unfortunate - but (probably) 
-- unavoidable deficiency.
--
-- The use of PostScript\'s @concat@ operator to alter the arc 
-- hw/hh will vary the line width during the drawing of a stroked 
-- ellipse.
--
-- For good stroked ellipses, Bezier curves constructed from 
-- PrimPaths should be used.
--
primEllipse :: EllipseProps -> PrimEllipse -> PsMonad Doc
primEllipse props (PrimEllipse hw hh ctm) 
    | hw == hh  = bracketPrimCTM ctm (drawC props)
    | otherwise = bracketPrimCTM ctm (drawE props)
  where
    drawC (EFill rgb)            pt = fillCircle rgb hw pt
    drawC (EStroke sa rgb)       pt = strokeCircle rgb sa hw pt
    drawC (EFillStroke fc sa sc) pt = 
        vconcat <$> fillCircle fc hw pt <*>  strokeCircle sc sa hw pt

    drawE (EFill rgb)            pt = fillEllipse rgb hw hh pt
    drawE (EStroke sa rgb)       pt = strokeEllipse rgb sa hw hh pt
    drawE (EFillStroke fc sa sc) pt = 
        vconcat <$> fillEllipse fc hw hh pt <*>  strokeEllipse sc sa hw hh pt
                       


-- This will need to become monadic to handle /colour delta/.
--
fillEllipse :: RGBi -> Double -> Double -> DPoint2 -> PsMonad Doc
fillEllipse rgb rx ry pt = 
    (\rgbd -> rgbd `vconcat` ps_wumpus_FELL pt rx ry)
      <$> deltaDrawColour rgb

strokeEllipse :: RGBi -> StrokeAttr -> Double -> Double -> DPoint2 -> PsMonad Doc
strokeEllipse rgb sa rx ry pt =
    (\rgbd attrd -> vcat [ rgbd
                         , attrd
                         , ps_wumpus_SELL pt rx ry ])
      <$> deltaDrawColour rgb <*> deltaStrokeAttrs sa


-- This will need to become monadic to handle /colour delta/.
--
fillCircle :: RGBi -> Double -> DPoint2 -> PsMonad Doc
fillCircle rgb r pt = 
    (\rgbd -> rgbd `vconcat` ps_wumpus_FCIRC pt r)
      <$> deltaDrawColour rgb

strokeCircle :: RGBi -> StrokeAttr -> Double -> DPoint2 -> PsMonad Doc
strokeCircle rgb sa r pt =
    (\rgbd attrd -> vcat [ rgbd
                         , attrd
                         , ps_wumpus_SCIRC pt r ])
      <$> deltaDrawColour rgb <*> deltaStrokeAttrs sa


-- Note - for the otherwise case, the x-and-y coordinates are 
-- encoded in the matrix, hence the @ 0 0 moveto @.
--
primLabel :: LabelProps -> PrimLabel -> PsMonad Doc
primLabel (LabelProps rgb attrs) (PrimLabel body _ ctm) = bracketPrimCTM ctm mf
  where
    ev    = font_enc_vector $ font_face attrs    
    mf pt = (\rgbd fontd -> vcat [ rgbd, fontd, labelBody ev pt body ]) 
              <$> deltaDrawColour rgb <*> deltaFontAttrs attrs 


-- Note - it would be nice to use @xshow@ and @yshow@ (PostScript
-- Language Level 2) to generate more efficient code as per using 
-- horizontal or vertical coordinate lists in the SVG output.
-- 
-- Unfortunately, because Wumpus has to use @ps_glyphshow@ for 
-- any special charaters the code cannot use necessarily use 
-- @xshow@ to generate a single string. If the string is 
-- broken by a special char calculating the restart position is 
-- complicated because PostScript implicitly moves the current 
-- point as a string is drawn (using the advance vector for each 
-- glyph in the font). 
-- 
-- I tried to use @xshow@ and @yshow@ in an experiment, but the 
-- Haskell code was very complicated and the generated PostScript 
-- was not much better in the presence of special chars. As 
-- Wumpus makes no claims for suitability regarding type-setting 
-- the current code seems good enough.
--
    

labelBody :: EncodingVector -> DPoint2 -> LabelBody -> Doc
labelBody ev pt (StdLayout txt) = ps_moveto pt `vconcat` psText ev txt
labelBody ev pt (KernTextH xs)  = kernTextH ev pt xs
labelBody ev pt (KernTextV xs)  = kernTextV ev pt xs

-- 
kernTextH :: EncodingVector -> DPoint2 -> [KerningChar] -> Doc
kernTextH ev pt0 xs = snd $ F.foldl' fn (pt0,empty) xs
  where
    fn (P2 x y,acc) (dx,ch) = let doc1 = psChar ev ch
                                  pt   = P2 (x+dx) y 
                              in (pt, vcat [acc, ps_moveto pt, doc1])


-- Note - vertical labels grow downwards...
--
kernTextV :: EncodingVector -> DPoint2 -> [KerningChar] -> Doc
kernTextV ev pt0 xs = snd $ F.foldl' fn (pt0,empty) xs
  where
    fn (P2 x y,acc) (dy,ch) = let doc1 = psChar ev ch
                                  pt   = P2 x (y-dy) 
                              in (pt, vcat [acc, ps_moveto pt, doc1])


--------------------------------------------------------------------------------
-- Stroke, font and drawing colour attribute delta

-- This needs more thought than SVG as there is no natural 
-- nesting to benefit from...

-- All graphics are annotated with colour - the graphics state
-- doesn\'t tell us the actual colour of anything, only if we 
-- need to write a colour change to the output.
-- 
-- So we compare the current colour with the state - if it is
-- different we put the new colour in the state and output a 
-- @setrgbcolor@ message.
--
--
-- Note - because these combinators return /empty/ for no 
-- difference some extraneous blank lines are produced in the 
-- output.
--

deltaDrawColour :: RGBi -> PsMonad Doc
deltaDrawColour rgb = getDrawColour >>= \inh -> 
   if rgb==inh then return empty
               else setDrawColour rgb >> return (ps_setrgbcolor rgb)


deltaStrokeAttrs :: StrokeAttr -> PsMonad Doc
deltaStrokeAttrs sa = 
    (\d1 d2 d3 d4 d5 -> vcat $ catMaybes [d1,d2,d3,d4,d5])  
      <$> lw <*> ml <*> lc <*> lj <*> dp
  where
    lw = let d = line_width sa in 
         getLineWidth >>= \inh -> 
         if d == inh 
              then return Nothing 
              else setLineWidth d >> return (Just $ ps_setlinewidth d)

    ml = let d = miter_limit sa in 
         getMiterLimit >>= \inh -> 
         if d==inh 
              then return Nothing 
              else setMiterLimit d >> return (Just $ ps_setmiterlimit d)
                            
    lc = let d = line_cap sa in
         getLineCap >>= \inh -> 
         if d==inh 
              then return Nothing
              else setLineCap d >> return (Just $ ps_setlinecap d)
                      
    lj = let d = line_join sa in 
         getLineJoin >>= \inh -> 
         if d==inh 
              then return Nothing
              else setLineJoin d >> return (Just $ ps_setlinejoin d)

    dp = let d = dash_pattern sa in
         getDashPattern >>= \inh -> 
         if d==inh 
               then return Nothing
               else setDashPattern d >> return (Just $ ps_setdash d)



deltaFontAttrs :: FontAttr -> PsMonad Doc
deltaFontAttrs fa  = getFontAttr >>= \inh ->
    if fa==inh then return empty 
               else setFontAttr fa >> return (makeFontAttrs fa)

makeFontAttrs :: FontAttr -> Doc
makeFontAttrs (FontAttr sz face) = ps_wumpus_FL sz (ps_font_name face)


--------------------------------------------------------------------------------
-- Bracket matrix and PrimCTM trafos

bracketTrafos :: [AffineTrafo] -> PsMonad Doc -> PsMonad Doc
bracketTrafos xs ma = bracketMatrix (concatTrafos xs) ma 

bracketMatrix :: Matrix3'3 Double -> PsMonad Doc -> PsMonad Doc
bracketMatrix mtrx ma 
    | mtrx == identityMatrix = ma
    | otherwise              = (\doc -> vcat [inn, doc, out]) <$> ma
  where
    inn   = ps_concat $ mtrx
    out   = ps_concat $ invert mtrx


bracketPrimCTM :: PrimCTM -> (DPoint2 -> PsMonad Doc) -> PsMonad Doc
bracketPrimCTM ctm0 mf = step $ unCTM ctm0 
  where 
    step (p0,ctm) 
      | ctm == identityCTM  = mf p0
      | otherwise           = let mtrx  = matrixRepCTM ctm0  -- originalCTM
                                  inn   = ps_concat $ mtrx
                                  out   = ps_concat $ invert mtrx
                              in (\doc -> vcat [inn, doc, out]) <$> mf zeroPt



