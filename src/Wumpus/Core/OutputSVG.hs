{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.OutputSVG
-- Copyright   :  (c) Stephen Tetley 2009-2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output SVG. 
--
-- Note - the coordinate systems of Wumpus and SVG are different.
--
-- > Wumpus - (0,0) is bottom-left.
--
-- > SVG - (0,0) is top-left.
--
-- To accommodate this, Wumpus adds rectifying matrix 
-- transformations to the generated SVG code.
--
--------------------------------------------------------------------------------

module Wumpus.Core.OutputSVG 
  (

  -- * Output SVG
    writeSVG

  , writeSVG_defs


  ) where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Colour
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicProps
import Wumpus.Core.PageTranslation
import Wumpus.Core.PictureInternal
import Wumpus.Core.SVGDoc
import Wumpus.Core.TrafoInternal
import Wumpus.Core.Text.Base
import Wumpus.Core.Text.GlyphIndices
import Wumpus.Core.Utils.FormatCombinators
import Wumpus.Core.Utils.JoinList

import Data.AffineSpace                         -- package: vector-space

import Data.Char
import Data.List ( mapAccumL )
import qualified Data.Map as Map
import Data.Maybe


-- DESIGN NOTE
--
-- SVG output is complicated by two differences with PostScript.
--
-- 1. The coordinate space of SVG is /origin top-left/, for 
-- PostScript it is /origin bottom-left/.
-- 
-- 2. Clipping in SVG uses /tagging/. A clipPath element is 
-- declared and named, subsequent elements within the clipping 
-- area reference it via the clip-path attribute - 
-- @clip-path=\"url(#clip_path_tag)\"@.
--


-- SvgMonad is two Readers plus Int state for clip paths...
--

type ClipCount = Int

newtype SvgMonad a = SvgMonad { 
          getSvgMonad :: GraphicsState -> ClipCount -> (a,ClipCount) }



instance Functor SvgMonad where
  fmap f mf = SvgMonad $ \r s -> let (a,s1) = getSvgMonad mf r s
                                 in (f a,s1)

instance Applicative SvgMonad where
  pure a    = SvgMonad $ \_ s -> (a,s)
  mf <*> ma = SvgMonad $ \r s -> let (f,s1) = getSvgMonad mf r s
                                     (a,s2) = getSvgMonad ma r s1
                                 in (f a, s2)

instance Monad SvgMonad where
  return a  = SvgMonad $ \_ s -> (a,s)
  m >>= k   = SvgMonad $ \r s -> let (a,s1) = getSvgMonad m r s
                                 in (getSvgMonad . k) a r s1
                            


runSvgMonad :: SvgMonad a -> a
runSvgMonad mf = fst $ getSvgMonad mf zeroGS 0

newClipLabel :: SvgMonad String
newClipLabel = SvgMonad $ \_ s -> ('c':'l':'i':'p':show s, s+1)



-- This is different to the PsMonad version, as SVG is nested 
-- (and /graphics state/ is via a Reader), so it is the same as 
-- local with a Reader monad.
--
runLocalGS :: (GraphicsState -> GraphicsState) -> SvgMonad a -> SvgMonad a
runLocalGS upd mf = 
    SvgMonad $ \r s -> getSvgMonad mf (upd r) s


askGraphicsState :: SvgMonad GraphicsState
askGraphicsState = SvgMonad $ \r s -> (r,s)

asksGraphicsState :: (GraphicsState -> a) -> SvgMonad a
asksGraphicsState fn = fmap fn askGraphicsState

askFontAttr     :: SvgMonad FontAttr
askFontAttr     = asksGraphicsState $ \r -> 
                    FontAttr (gs_font_size r) (gs_font_face r)

askLineWidth    :: SvgMonad Double
askLineWidth    = asksGraphicsState (line_width . gs_stroke_attr)

askMiterLimit   :: SvgMonad Double
askMiterLimit   = asksGraphicsState (miter_limit . gs_stroke_attr)

askLineCap      :: SvgMonad LineCap
askLineCap      = asksGraphicsState (line_cap . gs_stroke_attr)

askLineJoin     :: SvgMonad LineJoin
askLineJoin     = asksGraphicsState (line_join . gs_stroke_attr)

askDashPattern  :: SvgMonad DashPattern
askDashPattern  = asksGraphicsState (dash_pattern . gs_stroke_attr)


--------------------------------------------------------------------------------


svgChar :: EscapedChar -> Doc
svgChar (CharLiteral c) | ord c < 0x80  = char c
svgChar (CharLiteral c)                 = escapeSpecial $ ord c
svgChar (CharEscInt i)                  = escapeSpecial i
svgChar (CharEscName s)                 = 
   escapeSpecial $ fromMaybe 0x0020 $ Map.lookup s ps_glyph_indices 

--------------------------------------------------------------------------------

-- | Output a picture to a SVG file. 
--
writeSVG :: FilePath -> Picture -> IO ()
writeSVG filepath pic = 
    writeFile filepath $ show $ svgDraw Nothing pic 



-- | 'writeSVG_defs' : @ file_name -> defs -> picture -> IO () @
--
-- Output a picture to a SVG file the supplied /defs/ are
-- written into the defs section of SVG file verbatim. 
--
-- This is considered an experimental feature, use 'writeSVG' 
-- instead.
--
writeSVG_defs :: FilePath -> String -> Picture -> IO ()
writeSVG_defs filepath ss pic = 
    writeFile filepath $ show $ svgDraw (Just ss) pic 


svgDraw :: Maybe String -> Picture -> Doc
svgDraw mb_defs original_pic = 
    let pic          = svgPageTranslation original_pic
        (_,imgTrafo) = imageTranslation pic
        body         = runSvgMonad $ picture pic
        mkSvg        = maybe elem_svg elem_svg_defs mb_defs
    in vcat [ xml_version, doctype, mkSvg $ imgTrafo body ]



imageTranslation :: Picture -> (DBoundingBox, Doc -> Doc)
imageTranslation pic = case repositionDeltas pic of
  (bb, Nothing) -> (bb, id)
  (bb, Just v)  -> let attr = attr_transform (val_translate v) 
                   in (bb, elem_g attr)

--------------------------------------------------------------------------------

-- Note - might be simpler to only print a @Picture Double@

picture :: Picture -> SvgMonad Doc
picture (Leaf    (_,xs) ones)   = bracketTrafos xs $ oneConcat primitive ones
picture (Picture (_,xs) ones)   = bracketTrafos xs $ oneConcat picture ones


oneConcat :: (a -> SvgMonad Doc) -> JoinList a -> SvgMonad Doc
oneConcat fn ones = outstep (viewl ones)
  where
    outstep (e :< rest)   = fn e >>= \a -> instep a (viewl rest)
    outstep (OneL e)      = fn e
    
    instep ac (OneL e)    = fn e >>= \a -> return (ac `vconcat` a)
    instep ac (e :< rest) = fn e >>= \a -> instep (ac `vconcat` a) (viewl rest)


primitive :: Primitive -> SvgMonad Doc
primitive (PPath props pp)      
    | isEmptyPath pp            = pure empty
    | otherwise                 = primPath props pp

primitive (PLabel props lbl)    
    | isEmptyLabel lbl          = pure empty
    | otherwise                 = primLabel props lbl

primitive (PEllipse props ell)  = primEllipse props ell

primitive (PContext fa chi)     = bracketGS fa (primitive chi)

primitive (PSVG anno chi)       = svgAnnoPrim anno <$> primitive chi

primitive (PGroup ones)         = oneConcat primitive ones

primitive (PClip cp chi)        =  do 
    { lbl <- newClipLabel
    ; let d1 = clipPath lbl cp
    ; d2  <- primitive chi
    ; return (vconcat d1 (elem_g (attr_clip_path lbl) d2))
    } 


 

svgAnnoPrim :: SvgAnno -> Doc -> Doc
svgAnnoPrim (ALink hypl)    d = drawXLink hypl d
svgAnnoPrim (GAnno xs)      d = drawGProps xs d
svgAnnoPrim (SvgAG hypl xs) d = drawXLink hypl $ drawGProps xs d 
svgAnnoPrim (SvgId ss)      d = drawGProps [SvgAttr "id" ss] d 


drawXLink :: XLink -> Doc -> Doc
drawXLink (XLink href) doc = elem_a_xlink href doc

drawGProps :: [SvgAttr] -> Doc -> Doc
drawGProps [] d = d 
drawGProps xs d = elem_g attrs_doc d
  where
    attrs_doc = hsep $ map svgAttribute xs

svgAttribute :: SvgAttr -> Doc
svgAttribute (SvgAttr n v) = svgAttr n $ text v
 
clipPath :: String -> PrimPath -> Doc
clipPath clip_id pp = 
    elem_clipPath (attr_id clip_id) (elem_path_no_attrs $ path pp) 


primPath :: PathProps -> PrimPath -> SvgMonad Doc
primPath props pp = (\(a,f) -> elem_path a (f $ path pp)) <$> pathProps props

--
-- Paths are printed as absolute paths. Internally they are 
-- relative paths, but client code specifies them as absolute 
-- paths. So, here at least, the output matches the input.
-- 
-- Also, the SVG syntax for distinguishing between absolute and 
-- relative paths is is horrible (upper case char versus its 
-- corresponding lower case char). As Wumpus used absolute paths 
-- internally up to version 0.40.0, the horrible syntax was not
-- an encouragement to change when it moved to relative ones. 
-- 

path :: PrimPath -> Doc
path ppath = 
    let (start,xs) = extractRelPath ppath
    in path_m start <+> hsep (snd $ mapAccumL step start xs)
  where
    step pt (RelLineTo v)         = let p1 = pt .+^ v in (p1, path_l p1)
    step pt (RelCurveTo v1 v2 v3) = let p1 = pt .+^ v1 
                                        p2 = p1 .+^ v2
                                        p3 = p2 .+^ v3
                                    in (p3, path_c p1 p2 p3)


-- Return - drawing props, plus a function to close the path (or not). 
--
pathProps :: PathProps -> SvgMonad (Doc, Doc -> Doc)
pathProps props = fn props
  where
    fn (CFill rgb)                = pure (fillNotStroke rgb, close) 

    fn (CStroke attrs rgb)        = 
        (\a -> (strokeNotFill rgb <+> a, close))   <$> deltaStrokeAttrs attrs

    fn (OStroke attrs rgb)        = 
        (\a -> (strokeNotFill rgb <+> a, id))      <$> deltaStrokeAttrs attrs

    fn (CFillStroke fc attrs sc)  =
        (\a -> (fillAndStroke fc sc <+> a, close)) <$> deltaStrokeAttrs attrs

    fillNotStroke rgb             = attr_fill rgb   <+> attr_stroke_none 
    strokeNotFill rgb             = attr_stroke rgb <+> attr_fill_none
    fillAndStroke a b             = attr_fill a     <+> attr_stroke b
    close                         = (<+> char 'Z')
 



-- Note - if hw==hh then draw the ellipse as a circle.
--
primEllipse :: EllipseProps -> PrimEllipse -> SvgMonad Doc
primEllipse props (PrimEllipse hw hh ctm) 
    | hw == hh  = (\a b -> elem_circle (a <+> circle_radius <+> b))
                    <$> bracketEllipseCTM ctm mkCXCY <*> ellipseProps props
    | otherwise = (\a b -> elem_ellipse (a <+> ellipse_radius <+> b))
                    <$> bracketEllipseCTM ctm mkCXCY <*> ellipseProps props
  where
    mkCXCY (P2 x y) = pure $ attr_cx x <+> attr_cy y
   
    circle_radius   = attr_r hw
    ellipse_radius  = attr_rx hw <+> attr_ry hh

 

ellipseProps :: EllipseProps -> SvgMonad Doc
ellipseProps (EFill rgb)                   = 
    pure (attr_fill rgb <+> attr_stroke_none)

ellipseProps (EStroke attrs rgb)           = 
    (\a -> attr_stroke rgb <+> attr_fill_none <+> a)  <$> deltaStrokeAttrs attrs

ellipseProps (EFillStroke frgb attrs srgb) = 
    (\a -> attr_fill frgb <+> attr_stroke srgb <+> a) <$> deltaStrokeAttrs attrs



-- Note - SVG rendering coloured text seemed convoluted 
-- mandating the tspan element in the output. 
--
-- TO CHECK - is this really the case?
-- 
--

primLabel :: LabelProps -> PrimLabel -> SvgMonad Doc
primLabel (LabelProps rgb attrs) (PrimLabel body opt_id ctm) = 
    (\fa ca -> elem_text (id_f $ fa <+> ca) (makeTspan rgb dtext))
      <$> deltaFontAttrs attrs <*> bracketTextCTM ctm coordf
  where
    coordf = \p0 -> pure $ labelBodyCoords body p0
    dtext  = labelBodyText body
    id_f   = maybe id (\xid -> (svgAttr "id" (text xid) <+>)) $ opt_id

labelBodyCoords :: LabelBody -> DPoint2 -> Doc
labelBodyCoords (StdLayout _)  pt = makeXY pt
labelBodyCoords (KernTextH xs) pt = makeXsY pt xs        
labelBodyCoords (KernTextV xs) pt = makeXYs pt xs

labelBodyText :: LabelBody -> Doc
labelBodyText (StdLayout enctext) = encodedText enctext
labelBodyText (KernTextH xs)      = kerningText xs
labelBodyText (KernTextV xs)      = kerningText xs


encodedText :: EscapedText -> Doc
encodedText enctext = hcat $ destrEscapedText (map svgChar) enctext

kerningText :: [KerningChar] -> Doc
kerningText xs = hcat $ map (\(_,c) -> svgChar c) xs



makeTspan :: RGBi -> Doc -> Doc
makeTspan rgb body = elem_tspan (attr_fill rgb) body

makeXY :: DPoint2 -> Doc
makeXY (P2 x y) = attr_x x <+> attr_y y

-- This is for horizontal kerning text, the output is of the 
-- form:
-- 
-- > x="0 10 25 35" y="0"
--
makeXsY :: DPoint2 -> [KerningChar] -> Doc
makeXsY (P2 x y) ks = attr_xs (step x ks) <+> attr_y y
  where 
    step ax ((d,_):ds) = let a = ax+d in a : step a ds 
    step _  []         = []


-- This is for vertical kerning text, the output is of the 
-- form:
-- 
-- > x="0 0 0 0" y="0 10 25 35"
--
-- Note - this is different to the horizontal version as the 
-- x-coord needs to be /realigned/ at each step.
--
makeXYs :: DPoint2 -> [KerningChar] -> Doc
makeXYs (P2 x y) ks = attr_xs xcoords <+> attr_ys (step y ks)
  where 
    xcoords            = replicate (length ks) x
    step ay ((d,_):ds) = let a = ay+d in a : step a ds 
    step _  []         = []
    
    

--------------------------------------------------------------------------------
-- Stroke and font attribute delta

deltaStrokeAttrs :: StrokeAttr -> SvgMonad Doc
deltaStrokeAttrs sa = 
    (\d1 d2 d3 d4 d5 -> hsep $ catMaybes [d1,d2,d3,d4,d5])  
      <$> lw <*> ml <*> lc <*> lj <*> dp
  where
    lw = let d = line_width sa in
         askLineWidth >>= \inh -> 
         if d==inh then return Nothing 
                   else return (Just $ attr_stroke_width d) 
         
    ml = let d = miter_limit sa in
         askMiterLimit >>= \inh -> 
         if d==inh then return Nothing 
                   else return (Just $ attr_stroke_miterlimit d)

    lc = let d = line_cap sa in
         askLineCap >>= \inh -> 
         if d==inh then return Nothing 
                   else return (Just $ attr_stroke_linecap d)

    lj = let d = line_join sa in
         askLineJoin >>= \inh -> 
         if d==inh then return Nothing 
                   else return (Just $ attr_stroke_linejoin d)

    dp = let d = dash_pattern sa in 
         askDashPattern >>= \inh -> 
         if d==inh then return Nothing 
                   else return (Just $ makeDashPattern d) 

makeDashPattern :: DashPattern -> Doc
makeDashPattern Solid       = attr_stroke_dasharray_none
makeDashPattern (Dash n xs) = 
    attr_stroke_dashoffset n <+> attr_stroke_dasharray xs


data FontMatch = FullMatch | DeltaPtSize | DeltaFont
  deriving (Eq,Show,Ord) 


deltaFontAttrs :: FontAttr -> SvgMonad Doc
deltaFontAttrs fa = (\inh -> step $ fontMatch inh fa) <$> askFontAttr
  where
    step FullMatch      = empty
    step DeltaPtSize    = attr_font_size $ font_size fa
    step DeltaFont      = makeFontAttrs fa


fontMatch :: FontAttr -> FontAttr -> FontMatch
fontMatch (FontAttr s1 f1) (FontAttr s2 f2) 
   | s1 == s2 && f1 == f2 = FullMatch
   | f1 == f2             = DeltaPtSize
   | otherwise            = DeltaFont

-- Note this is always adding FontSize - there are cases where 
-- this is redundant. 
--
makeFontAttrs :: FontAttr -> Doc
makeFontAttrs (FontAttr sz face) = 
    attr_font_family (svg_font_family face) <+> attr_font_size sz 
                                            >< suffix (svg_font_style face) 
  where  
    suffix SVG_REGULAR      = empty

    suffix SVG_BOLD         = space >< attr_font_weight "bold"

    suffix SVG_ITALIC       = space >< attr_font_style "italic"

    suffix SVG_BOLD_ITALIC  = 
        space >< attr_font_weight "bold" <+> attr_font_style "italic"

    suffix SVG_OBLIQUE      = space >< attr_font_style "oblique"

    suffix SVG_BOLD_OBLIQUE = 
        space >< attr_font_weight "bold" <+> attr_font_style "oblique"



-- Always update both the size and font-family even if only one
-- changes.
--
-- This seems more in the spirit of a font delta operation.
--
bracketGS :: FontCtx -> SvgMonad Doc -> SvgMonad Doc
bracketGS (FontCtx new_font) mf = 
    (\old body -> mkElem (old == new_font) body) 
        <$> askFontAttr <*> runLocalGS updateF mf
  where
    mkElem True body = elem_g_no_attrs body
    mkElem _    body = let a = makeFontAttrs new_font in elem_g a body

    updateF s = s { gs_font_size = font_size new_font
                  , gs_font_face = font_face new_font }
                


--------------------------------------------------------------------------------
-- Bracket matrix and PrimCTM trafos

bracketTrafos :: [AffineTrafo] -> SvgMonad Doc -> SvgMonad Doc
bracketTrafos xs ma = bracketMatrix (concatTrafos xs) ma 

bracketMatrix :: Matrix3'3 Double -> SvgMonad Doc -> SvgMonad Doc
bracketMatrix mtrx ma 
    | mtrx == identityMatrix = (\doc -> elem_g_no_attrs doc) <$> ma
    | otherwise              = (\doc -> elem_g trafo doc)    <$> ma
  where
    trafo = attr_transform $ val_matrix mtrx


-- Note - there are versions of the /same/ function for text and 
-- ellipses.
-- 
-- For text we always want a matrix transformation in the 
-- generated SVG - wumpus has flipped the page coordinates, so
-- it must flip text accordingly.
--
-- For ellipses and circles we dont\'t have to bother with the
-- rectifying flip transformation /if/ the ellipse or circle has 
-- not been scaled or rotated.
--
bracketTextCTM :: PrimCTM -> (DPoint2 -> SvgMonad Doc) -> SvgMonad Doc
bracketTextCTM ctm0 pf = (\xy -> xy <+> mtrx) <$> pf zeroPt
  where
    mtrx = attr_transform $ val_matrix $ matrixRepCTM ctm0



-- Note - the otherwise step uses the original ctm (ctm0).
-- 
-- Note v0.41.0 otherwise step always fires because the matrix 
-- has been transformed for SVG coordspace to [1,0,0,-1].
--
bracketEllipseCTM :: PrimCTM -> (DPoint2 -> SvgMonad Doc) -> SvgMonad Doc
bracketEllipseCTM ctm0 pf = step $ unCTM ctm0
  where
    step (p0, ctm) 
        | ctm == flippedCTM   = pf p0
        | otherwise           = let mtrx = attr_transform $ 
                                             val_matrix $ matrixRepCTM ctm0
                                in (\xy -> xy <+> mtrx) <$> pf zeroPt


flippedCTM :: PrimCTM
flippedCTM = PrimCTM { ctm_trans_x = 0
                     , ctm_trans_y = 0
                     , ctm_scale_x = 1
                     , ctm_scale_y = (-1)
                     , ctm_rotation = 0 
                     }

