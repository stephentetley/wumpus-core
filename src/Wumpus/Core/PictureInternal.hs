{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PictureInternal
-- Copyright   :  (c) Stephen Tetley 2009-2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Internal representation of Pictures.
--
--------------------------------------------------------------------------------


module Wumpus.Core.PictureInternal
  ( 

    Picture(..)
  , Locale
  , FontCtx(..)

  , Primitive(..)
  , SvgAnno(..)
  , XLink(..)
  , SvgAttr(..)

  , PrimPath(..)
  , PrimPathSegment(..)
  , AbsPathSegment(..)
  , PrimLabel(..)
  , LabelBody(..)
  , KerningChar
  , PrimEllipse(..)

  , GraphicsState(..)

  , mapLocale

  -- * Additional operations
  , concatTrafos
  , deconsMatrix
  , repositionDeltas
  , extractRelPath

  , zeroGS
  , isEmptyPath
  , isEmptyLabel

  , pushXIdAnno

  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.BoundingBox
import Wumpus.Core.Colour
import Wumpus.Core.FontSize
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicProps
import Wumpus.Core.Text.Base
import Wumpus.Core.TrafoInternal
import Wumpus.Core.Utils.FormatCombinators
import Wumpus.Core.Utils.JoinList


import Data.AffineSpace                         -- package: vector-space

import qualified Data.Foldable                  as F
import qualified Data.IntMap                    as IntMap


-- | Picture is a rose tree. Leaves themselves are attributed
-- with colour, line-width etc. The /unit/ of a Picture is 
-- fixed to Double representing PostScript\'s /Point/ unit. 
-- Output is always gewnerated with PostScript points - other
-- units are converted to PostScript points before building the 
-- Picture.
-- 
-- By attributing leaves with their drawing properties, Wumpus\'s 
-- picture representaion is not directly matched to PostScript.
-- PostScript has a global graphics state (that allows local 
-- modifaction) from where drawing properties are inherited.
-- Wumpus has no attribute inheritance.
--
-- Omitting some details of the list representation, Picture is a 
-- simple non-empty rose tree via:
-- 
-- > tree = Leaf [primitive] | Picture [tree]
--
data Picture = Leaf     Locale  (JoinList Primitive)
             | Picture  Locale  (JoinList Picture)
  deriving (Show)

type instance DUnit Picture = Double

-- | Locale = (bounding box * current translation matrix)
-- 
-- Pictures (and sub-pictures) are located frame consisting of a 
-- bounding box and a translation matrix (represented as a list 
-- of affine transformations). So that pictures can be arranged 
-- via vertical and horizontal composition their bounding box is 
-- cached.
--
-- In Wumpus, affine transformations (scalings, rotations...)
-- transform the CTM rather than the constituent points of 
-- the primitives. Changes of CTM are transmitted to PostScript
-- as @concat@ commands (and matrix transforms in SVG).
--  
-- So that picture composition is remains stable under affine
-- transformation, the corners of bounding boxes are transformed
-- pointwise when the picture is scaled, rotated etc.
--
type Locale = (BoundingBox Double, [AffineTrafo])



-- | Wumpus\'s drawings are built from two fundamental 
-- primitives: paths (straight line segments and Bezier curves) 
-- and labels (single lines of text). 
-- 
-- Ellipses are a included as a primitive only for optimization 
-- - drawing a reasonable circle with Bezier curves needs at 
-- least eight curves. This is inconvenient for drawing dots 
-- which can otherwise be drawn with a single @arc@ command.
-- 
-- Wumpus does not follow PostScript employing arc as a general 
-- path primitive - arcs are used only to draw ellipses. This 
-- is because arcs do not enjoy the nice properties of Bezier 
-- curves, whereby the affine transformation of a Bezier curve 
-- can simply be achieved by the affine transformation of it\'s 
-- control points.
--
-- Ellipses are represented by their center, half-width and 
-- half-height. Half-width and half-height are used so the 
-- bounding box can be calculated using only multiplication, and 
-- thus initially only obliging a Num constraint on the unit.
-- Though typically for affine transformations a Fractional 
-- constraint is also obliged.
--
-- Clipping is represented by a pair of the clipping path and
-- the primitive embedded within the path.
--
-- To represent XLink hyperlinks, Primitives can be annotated 
-- with some a hyperlink (likewise a /passive/ font change for 
-- better SVG code generation) and grouped - a hyperlinked arrow 
-- would want the tip and the arrow body both to be incorporated 
-- in thelink even though they are two drawing primitives. 
--
-- This means that Primitives aren\'t strictly /primitive/ as 
-- the actual implementation is a tree.
-- 
data Primitive = PPath    PathProps         PrimPath
               | PLabel   LabelProps        PrimLabel
               | PEllipse EllipseProps      PrimEllipse
               | PContext FontCtx           Primitive
               | PSVG     SvgAnno           Primitive
               | PGroup   (JoinList Primitive)
               | PClip    PrimPath          Primitive
  deriving (Eq,Show)

type instance DUnit Primitive = Double


-- | Set the font /delta/ for SVG rendering. 
-- 
-- Note - this does not change the default colour or font style. 
-- It is solely a backdoor into the SVG renderer to potential 
-- allow some code size reductions.
--
newtype FontCtx = FontCtx { getFontCtx :: FontAttr }
  deriving (Eq,Show)


-- | SVG annotations - annotations can be: 
-- 
-- * A hyperlink inside @<a ...> ... </a>@ .
--
-- * A group - @<g ...> ... </g>@
--
-- * A group of annotations inside a hyperlink.
--
-- * An @id@.
--
data SvgAnno = ALink XLink
             | GAnno [SvgAttr]
             | SvgAG XLink [SvgAttr]
             | SvgId String
   deriving (Eq,Show)


-- | Primitives can be grouped with hyperlinks in SVG output.
--
-- Note - this is always printed as @xlink:href="..."@. Other
-- types of xlink can be modelled with the unrestrained 
-- SvgAnno type.
--
newtype XLink = XLink { getXLink :: String }
  deriving (Eq,Show)


-- | Primitives can be labelled with arbitrary SVG properties 
-- (e.g @onmouseover@) within a group element.
--
-- Note - annotations should be used only for non-graphical 
-- properties. Graphical properties (fill_colour, font_size, etc.)
-- should be set through the appropriate Wumpus functions.
--
-- Also note, this functionality is has not been widely used. It
-- might be something of a white elephant.
--
data SvgAttr = SvgAttr 
      { svg_attr_name   :: String
      , svg_attr_value  :: String 
      }
  deriving (Eq,Show)


-- | PrimPath - a list of path segments and a CTM (translation 
-- matrix).
-- 
-- The start point of the path forms the (dx,dy) of the CTM. The 
-- CTM is otherwise hidden from the public constructors of this 
-- data type.
-- 
-- Note - the PrimPath type does not support concatenation.
-- It is expected that all PrimPaths will be created /in one go/,
-- and client code defines a higher-level path type that supports
-- concatenation, splitting etc.
--
-- Primitively paths can be built like this:
--
-- > 
-- > path1 :: PrimPath
-- > path1 = absPrimPath zeroPt [ absLineTo  (P2 0 60) 
-- >                            , absLineTo  (P2 40 100)
-- >                            , absLineTo  (P2 80 60)
-- >                            , absLineTo  (P2 80 0)
-- >                            , absLineTo  (P2 60 0)  
-- >                            , absLineTo  (P2 60 30)
-- >                            , absCurveTo (P2 60 50) (P2 50 60) (P2 40 60)
-- >                            , absCurveTo (P2 30 60) (P2 20 50) (P2 20 30)
-- >                            , absLineTo  (P2 20 0)
-- >                            ]
-- >
--
-- Although it\'s generally expected that PrimPaths will be 
-- constructed by traversing a higher-level path object and 
-- collecting calls to the @absCurevTo@ and @absLineTo@ functions
-- in a list.
-- 
data PrimPath = PrimPath [PrimPathSegment] PrimCTM
  deriving (Eq,Show)

type instance DUnit PrimPath = Double


-- | PrimPathSegment - either a relative cubic Bezier /curve-to/ 
-- or a relative /line-to/.
--
data PrimPathSegment = RelCurveTo  DVec2 DVec2 DVec2
                     | RelLineTo   DVec2
  deriving (Eq,Show)

type instance DUnit PrimPathSegment = Double


-- | AbsPathSegment - either a cubic Bezier curve or a line.
-- 
-- Note this data type is transitory - it is only used as a 
-- convenience to build relative paths. Hence the unit type is 
-- parametric.
--
data AbsPathSegment = AbsCurveTo  DPoint2 DPoint2 DPoint2
                    | AbsLineTo   DPoint2 
  deriving (Eq,Show)


type instance DUnit AbsPathSegment = Double


-- | Label - represented by baseline-left point and text.
--
-- Baseline-left is the dx * dy of the PrimCTM.
--
--
data PrimLabel = PrimLabel 
      { label_body      :: LabelBody
      , label_opt_id    :: Maybe String
      , label_ctm       :: PrimCTM
      }
  deriving (Eq,Show)

type instance DUnit PrimLabel = Double


-- | Label can be draw with 3 layouts.
-- 
-- The standard layout uses @show@ for PostScript and a single 
-- initial point for SVG.
--
-- Kerned horizontal layout - each character is encoded with the
-- rightwards horizontal distance from the last charcaters left 
-- base-line.
-- 
-- Kerned vertical layout - each character is encoded with the
-- upwards distance from the last charcaters left base-line.
-- 
data LabelBody = StdLayout EscapedText
               | KernTextH [KerningChar]
               | KernTextV [KerningChar]
  deriving (Eq,Show)

type instance DUnit LabelBody = Double


-- | A Char (possibly escaped) paired with its displacement from 
-- the previous KerningChar.
--
type KerningChar = (Double,EscapedChar) 


-- | Ellipse represented by center and half_width * half_height.
--
-- Center is the dx * dy of the PrimCTM.
--
data PrimEllipse = PrimEllipse 
      { ellipse_half_width    :: !Double
      , ellipse_half_height   :: !Double
      , ellipse_ctm           :: PrimCTM
      } 
  deriving (Eq,Show)

type instance DUnit PrimEllipse = Double

--
-- Design note - the CTM unit type is fixed to Double (PS point) 
-- rather than parametric on unit.
--
-- For the rationale see the PrimLabel design note.
-- 
 

--------------------------------------------------------------------------------
-- Graphics state datatypes

-- | Graphics state used by the rendering monads.
--
-- This type is hidden by the top-level module @Wumpus.Core@.
--
data GraphicsState = GraphicsState
      { gs_draw_colour  :: RGBi
      , gs_font_size    :: Int
      , gs_font_face    :: FontFace
      , gs_stroke_attr  :: StrokeAttr 
      }
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- instances

-- format

instance Format Picture where
  format (Leaf m prims)     = indent 2 $ vcat [ text "** Leaf-pic **"
                                              , fmtLocale m 
                                              , fmtPrimlist prims ]

  format (Picture m pics)   = indent 2 $ vcat [ text "** Tree-pic **"
                                              , fmtLocale m
                                              , fmtPics pics ]
 

fmtPics :: JoinList Picture -> Doc
fmtPics ones = snd $ F.foldl' fn (0,empty) ones
  where
    fn (n,acc) e = (n+1, vcat [ acc, text "-- " <+> int n, format e, line])


fmtLocale :: Locale -> Doc
fmtLocale (bb,_) = format bb


instance Format Primitive where
  format (PPath props p)    = 
      indent 2 $ vcat [ text "path:" <+> format props, format p ]

  format (PLabel props l)   =
      indent 2 $ vcat [ text "label:" <+> format props, format l ]

  format (PEllipse props e) = 
      indent 2 $ vcat [ text "ellipse:" <+> format props, format e ]

  format (PContext _ a)     = 
      vcat [ text "-- svg ctx change " , format a ]

  format (PSVG _ a)       = 
      vcat [ text "-- svg:", format  a ]

  format (PGroup ones)      = 
      vcat [ text "-- group ", fmtPrimlist ones  ]

  format (PClip path pic)  = 
      vcat [ text "-- clip-path ", format path, format pic  ]



fmtPrimlist :: JoinList Primitive -> Doc
fmtPrimlist ones = snd $ F.foldl' fn (0,empty) ones
  where
    fn (n,acc) e = (n+1, vcat [ acc, text "-- leaf" <+> int n, format e, line])


instance Format PrimPath where
   format (PrimPath vs ctm) = vcat [ hcat $ map format vs
                                   , text "ctm=" >< format ctm ]

instance Format PrimPathSegment where
  format (RelCurveTo p1 p2 p3)  =
    text "rel_curve_to " >< format p1 <+> format p2 <+> format p3

  format (RelLineTo pt)         = text "rel_line_to  " >< format pt

instance Format PrimLabel where
  format (PrimLabel s opt_id ctm) = 
     vcat [ dquotes (format s)
          , maybe (char '_') text $ opt_id 
          , text "ctm="           >< format ctm
          ]

instance Format LabelBody where
  format (StdLayout enctext) = format enctext
  format (KernTextH xs)      = text "(KernH)" <+> hcat (map (format .snd) xs)
  format (KernTextV xs)      = text "(KernV)" <+> hcat (map (format .snd) xs)


instance Format PrimEllipse where
  format (PrimEllipse hw hh ctm) =  text "hw="       >< format hw
                                <+> text "hh="       >< format hh
                                <+> text "ctm="      >< format ctm
  

instance Format XLink where
  format (XLink ss) = text "xlink:href" <+> text ss


--------------------------------------------------------------------------------

instance Boundary Picture where
  boundary = boundaryPicture

boundaryPicture :: Picture -> BoundingBox Double
boundaryPicture (Leaf    (bb,_) _)   = bb
boundaryPicture (Picture (bb,_) _)   = bb


instance Boundary Primitive where
  boundary = boundaryPrimitive

boundaryPrimitive :: Primitive -> BoundingBox Double
boundaryPrimitive (PPath _ p)      = boundaryPrimPath p
boundaryPrimitive (PLabel a l)     = labelBoundary (label_font a) l
boundaryPrimitive (PEllipse _ e)   = ellipseBoundary e
boundaryPrimitive (PContext _ a)   = boundaryPrimitive a
boundaryPrimitive (PSVG _ a)       = boundaryPrimitive a
boundaryPrimitive (PClip p _)      = boundaryPrimPath p
boundaryPrimitive (PGroup ones)    = outer $ viewl ones 
  where
    outer (OneL a)     = boundaryPrimitive a
    outer (a :< as)    = inner (boundaryPrimitive a) (viewl as)

    inner bb (OneL a)  = bb `boundaryUnion` boundaryPrimitive a
    inner bb (a :< as) = inner (bb `boundaryUnion` boundaryPrimitive a) 
                               (viewl as)


instance Boundary PrimPath where
  boundary = boundaryPrimPath


boundaryPrimPath :: PrimPath -> BoundingBox Double
boundaryPrimPath (PrimPath vs ctm) = 
    retraceBoundary (m33 *#) $ step zeroPt (zeroPt,zeroPt) vs
  where
    m33         = matrixRepCTM ctm

    step _  (lo,hi) []                         = BBox lo hi 

    step pt (lo,hi) (RelLineTo v1:rest)        = 
        let p1 = pt .+^ v1
        in step p1 (lo2 lo p1, hi2 hi p1) rest

    step pt (lo,hi) (RelCurveTo v1 v2 v3:rest) = 
        let p1  = pt .+^ v1
            p2  = p1 .+^ v2
            p3  = p2 .+^ v3
            lo' = lo4 lo p1 p2 p3 
            hi' = hi4 hi p1 p2 p3
        in step p3 (lo',hi') rest 

    lo2 (P2 x1 y1) (P2 x2 y2) = P2 (min x1 x2) (min y1 y2)

    hi2 (P2 x1 y1) (P2 x2 y2) = P2 (max x1 x2) (max y1 y2)

    lo4 (P2 x1 y1) (P2 x2 y2) (P2 x3 y3) (P2 x4 y4) = 
        P2 (min x1 $ min x2 $ min x3 x4) (min y1 $ min y2 $ min y3 y4) 

    hi4 (P2 x1 y1) (P2 x2 y2) (P2 x3 y3) (P2 x4 y4) = 
        P2 (max x1 $ max x2 $ max x3 x4) (max y1 $ max y2 $ max y3 y4) 
 


labelBoundary :: FontAttr -> PrimLabel -> BoundingBox Double
labelBoundary attr (PrimLabel body _ ctm) = 
    retraceBoundary (m33 *#) untraf_bbox
  where
    m33         = matrixRepCTM ctm
    untraf_bbox = labelBodyBoundary (font_size attr) body

labelBodyBoundary :: FontSize -> LabelBody -> BoundingBox Double
labelBodyBoundary sz (StdLayout etxt) = stdLayoutBB sz etxt
labelBodyBoundary sz (KernTextH xs)   = hKerningBB sz xs
labelBodyBoundary sz (KernTextV xs)   = vKerningBB sz xs


stdLayoutBB :: FontSize -> EscapedText -> BoundingBox Double
stdLayoutBB sz etxt = textBoundsEsc sz zeroPt etxt


-- Note - this assumes positive deltas (and a nonempty list)...
--
-- Kern deltas are relative to the left basepoint, so they are
-- irrespective of the actual charater width. Thus to calculate
-- the bounding box Wumpus calculates the bounds of one character
-- then expands the right edge with the sum of the (rightwards)
-- displacements.
-- 
hKerningBB :: FontSize -> [(Double,EscapedChar)] -> BoundingBox Double
hKerningBB sz xs = rightGrow (sumDiffs xs) $ textBounds sz zeroPt "A"
  where
    sumDiffs                          = foldr (\(u,_) i -> i+u)  0
    rightGrow u (BBox ll (P2 x1 y1))  = BBox ll (P2 (x1+u) y1)


-- Note - likewise same assumptions as horizontal version.
-- (A postive distance represents a move downwards)...
--
-- The kern delta is the distance between baselines of successive
-- characters, so character height is irrespective when summing 
-- the deltas.
-- 
-- Also note, that the Label /grows/ downwards...
--
vKerningBB :: FontSize -> [(Double,EscapedChar)] -> BoundingBox Double
vKerningBB sz xs = downGrow (sumDiffs xs) $ textBounds sz zeroPt "A"
  where
    sumDiffs                                = foldr (\(u,_) i -> i+u)  0
    downGrow u (BBox (P2 x0 y0) (P2 x1 y1)) = BBox (P2 x0 (y0-u)) (P2 x1 y1)


-- | Ellipse bbox is the bounding rectangle, rotated as necessary 
-- then retraced.
--
ellipseBoundary :: PrimEllipse -> BoundingBox Double
ellipseBoundary (PrimEllipse hw hh ctm) = 
    traceBoundary $ map (m33 *#) [sw,se,ne,nw]
  where
    sw   = P2 (-hw) (-hh) 
    se   = P2   hw  (-hh) 
    ne   = P2   hw    hh 
    nw   = P2 (-hw)   hh 
    m33  = matrixRepCTM ctm


--------------------------------------------------------------------------------
-- Affine transformations

-- Affine transformation of Pictures only transforms the 
-- BoundingBox, the primitives within the picture are untouched.
-- The transformation is transmitted to PostScript as a matrix 
-- update (frame change).
--

instance Transform Picture where
  transform mtrx = 
    mapLocale $ \(bb,xs) -> let cmd = Matrix mtrx
                            in (transform mtrx bb, cmd : xs)

instance Rotate Picture where
  rotate theta = 
    mapLocale $ \(bb,xs) -> (rotate theta bb, Rotate theta:xs)


instance RotateAbout Picture where
  rotateAbout theta pt = 
    mapLocale $ \(bb,xs) -> let cmd = RotAbout theta pt
                            in (rotateAbout theta pt bb, cmd : xs)

instance Scale Picture where
  scale sx sy = 
    mapLocale $ \(bb,xs) -> let cmd = Scale sx sy
                            in (scale sx sy bb, cmd : xs)

instance Translate Picture where
  translate dx dy = 
    mapLocale $ \(bb,xs) -> let cmd = Translate dx dy
                            in (translate dx dy bb, cmd : xs)
                     


mapLocale :: (Locale -> Locale) -> Picture -> Picture
mapLocale f (Leaf lc ones)     = Leaf (f lc) ones
mapLocale f (Picture lc ones)  = Picture (f lc) ones


--------------------------------------------------------------------------------
-- Transform primitives


-- Note - Primitives are not instances of transform
--
-- (ShapeCTM is not a real matrix).
-- 

instance Rotate Primitive where
  rotate r (PPath a path)   = PPath a    $ rotatePath r path
  rotate r (PLabel a lbl)   = PLabel a   $ rotateLabel r lbl
  rotate r (PEllipse a ell) = PEllipse a $ rotateEllipse r ell
  rotate r (PContext a chi) = PContext a $ rotate r chi 
  rotate r (PSVG a chi)     = PSVG a     $ rotate r chi 
  rotate r (PGroup xs)      = PGroup     $ fmap (rotate r) xs
  rotate r (PClip p chi)    = PClip (rotatePath r p) (rotate r chi)

instance RotateAbout Primitive where
  rotateAbout ang p0 (PPath a path)   = 
      PPath a    $ rotateAboutPath ang p0 path

  rotateAbout ang  p0 (PLabel a lbl)   = 
      PLabel a   $ rotateAboutLabel ang p0 lbl

  rotateAbout ang p0 (PEllipse a ell) = 
      PEllipse a $ rotateAboutEllipse ang p0 ell

  rotateAbout ang p0 (PContext a chi) = 
      PContext a $ rotateAbout ang p0 chi

  rotateAbout ang p0 (PSVG a chi)     = 
      PSVG a     $ rotateAbout ang p0 chi

  rotateAbout ang p0 (PGroup xs)      = 
      PGroup     $ fmap (rotateAbout ang p0) xs

  rotateAbout ang p0 (PClip p chi)    = 
      PClip (rotateAboutPath ang p0 p) (rotateAbout ang p0 chi)


instance Scale Primitive where
  scale sx sy (PPath a path)    = PPath a    $ scalePath sx sy path
  scale sx sy (PLabel a lbl)    = PLabel a   $ scaleLabel sx sy lbl
  scale sx sy (PEllipse a ell)  = PEllipse a $ scaleEllipse sx sy ell
  scale sx sy (PContext a chi)  = PContext a $ scale sx sy chi
  scale sx sy (PSVG a chi)      = PSVG a     $ scale sx sy chi
  scale sx sy (PGroup xs)       = PGroup     $ fmap (scale sx sy) xs
  scale sx sy (PClip p chi)     = PClip (scalePath sx sy p) (scale sx sy chi)


instance Translate Primitive where
  translate dx dy (PPath a path)   = 
      PPath a    $ translatePath dx dy path

  translate dx dy (PLabel a lbl)   = 
      PLabel a   $ translateLabel dx dy lbl

  translate dx dy (PEllipse a ell) = 
      PEllipse a $ translateEllipse dx dy ell

  translate dx dy (PContext a chi) = 
      PContext a $ translate dx dy chi

  translate dx dy (PSVG a chi)     = 
      PSVG a     $ translate dx dy chi

  translate dx dy (PGroup xs)      = 
      PGroup     $ fmap (translate dx dy) xs

  translate dx dy (PClip p chi)    = 
      PClip (translatePath dx dy p) (translate dx dy chi)


--------------------------------------------------------------------------------
-- Paths

-- Affine transformations on paths are applied to their control
-- points. 

rotatePath :: Radian -> PrimPath -> PrimPath
rotatePath ang (PrimPath vs ctm) = PrimPath vs (rotateCTM ang ctm)


rotateAboutPath :: Radian -> DPoint2 -> PrimPath -> PrimPath
rotateAboutPath ang (P2 x y) (PrimPath vs ctm) = 
    PrimPath vs (rotateAboutCTM ang (P2 x y) ctm)


scalePath :: Double -> Double -> PrimPath -> PrimPath
scalePath sx sy (PrimPath vs ctm) = PrimPath vs (scaleCTM sx sy ctm)


-- Note - translate only needs change the start point /because/ 
-- the path represented as a relative path.
-- 
translatePath :: Double -> Double -> PrimPath -> PrimPath
translatePath dx dy (PrimPath vs ctm) = 
    PrimPath vs (translateCTM dx dy ctm)


--------------------------------------------------------------------------------
-- Labels



-- Rotate the baseline-left start point _AND_ the CTM of the 
-- label.
--
rotateLabel :: Radian -> PrimLabel -> PrimLabel
rotateLabel ang (PrimLabel txt opt_id ctm) = 
    PrimLabel txt opt_id (rotateCTM ang ctm)


-- /rotateAbout/ the start-point, /rotate/ the the CTM.
--
rotateAboutLabel :: Radian -> DPoint2 -> PrimLabel -> PrimLabel
rotateAboutLabel ang (P2 x y) (PrimLabel txt opt_id ctm) = 
    PrimLabel txt opt_id (rotateAboutCTM ang (P2 x y) ctm)


scaleLabel :: Double -> Double -> PrimLabel -> PrimLabel
scaleLabel sx sy (PrimLabel txt opt_id ctm) = 
    PrimLabel txt opt_id (scaleCTM sx sy ctm)


-- Change the bottom-left corner.
--
translateLabel :: Double -> Double -> PrimLabel -> PrimLabel
translateLabel dx dy (PrimLabel txt opt_id ctm) = 
    PrimLabel txt opt_id (translateCTM dx dy ctm)

--------------------------------------------------------------------------------
-- Ellipse


rotateEllipse :: Radian -> PrimEllipse -> PrimEllipse
rotateEllipse ang (PrimEllipse hw hh ctm) = 
    PrimEllipse hw hh (rotateCTM ang ctm)
    

rotateAboutEllipse :: Radian -> DPoint2 -> PrimEllipse -> PrimEllipse
rotateAboutEllipse ang (P2 x y) (PrimEllipse hw hh ctm) = 
    PrimEllipse hw hh (rotateAboutCTM ang (P2 x y) ctm)


scaleEllipse :: Double -> Double -> PrimEllipse -> PrimEllipse
scaleEllipse sx sy (PrimEllipse hw hh ctm) = 
    PrimEllipse hw hh (scaleCTM sx sy ctm)
    


-- Change the point
--
translateEllipse :: Double -> Double -> PrimEllipse -> PrimEllipse
translateEllipse dx dy (PrimEllipse hw hh ctm) = 
    PrimEllipse hw hh (translateCTM dx dy ctm)



--------------------------------------------------------------------------------
-- Additional operations


-- | Destructor for Matrix3'3.
-- 
-- Pattern matching on 6-tuple may be more convenient than using 
-- the Matrix3'3 directly.
--
-- > (M3'3 e0x e1x ox  
-- >       e0y e1y oy  
-- >       _   _   _  ) = (e0x,e0y,  e1x,e1y,  ox,oy)
--  
deconsMatrix :: Matrix3'3 u -> (u,u,u,u,u,u)
deconsMatrix (M3'3 e0x e1x ox  
                   e0y e1y oy  
                   _   _   _  ) = (e0x,e0y,  e1x,e1y,  ox,oy)



-- If a picture has coordinates smaller than (P2 4 4) especially
-- negative ones then it needs repositioning before it is drawn 
-- to PostScript or SVG.
-- 
-- (P2 4 4) gives a 4 pt margin - maybe it sould be (0,0) or 
-- user defined.
--
repositionDeltas :: Picture -> (BoundingBox Double, Maybe DVec2)
repositionDeltas = step . boundaryPicture
  where
    step bb@(BBox (P2 llx lly) (P2 urx ury))
        | llx < 4 || lly < 4  = (BBox ll ur, Just $ V2 x y)
        | otherwise           = (bb, Nothing)
      where 
        x  = 4 - llx
        y  = 4 - lly
        ll = P2 (llx+x) (lly+y)
        ur = P2 (urx+x) (ury+y) 


extractRelPath :: PrimPath -> (DPoint2, [PrimPathSegment])
extractRelPath (PrimPath ss ctm) = (start, usegs)
  where 
    (start,dctm)  = unCTM ctm
    mtrafo        = transform (matrixRepCTM dctm)
    usegs         = map fn ss
    
    fn (RelCurveTo v1 v2 v3) = RelCurveTo (mtrafo v1) (mtrafo v2) (mtrafo v3)
    fn (RelLineTo v1)        = RelLineTo  (mtrafo v1)



--------------------------------------------------------------------------------

-- | The initial graphics state.
--
-- PostScript has no default font so we always want the first 
-- /delta/ operation not to find a match and cause a @findfont@
-- command to be generated (PostScript @findfont@ commands are 
-- only written in the output on /deltas/ to reduce the 
-- output size).
--
zeroGS ::  GraphicsState 
zeroGS = GraphicsState { gs_draw_colour  = black
                       , gs_font_size    = (-1)
                       , gs_font_face    = unmatchable_face
                       , gs_stroke_attr  = default_stroke_attr
                       }
  where
    unmatchable_face = FontFace "DONT_MATCH"     "" 
                                SVG_BOLD_OBLIQUE no_encoding

    no_encoding      = IntMap.empty 


-- | Is the path empty - if so we might want to avoid printing it.
--
isEmptyPath :: PrimPath -> Bool
isEmptyPath (PrimPath xs _) = null xs

-- | Is the label empty - if so we might want to avoid printing it.
--
isEmptyLabel :: PrimLabel -> Bool
isEmptyLabel (PrimLabel txt _ _) = body txt
   where
     body (StdLayout esc) = destrEscapedText null esc
     body (KernTextH xs)  = null xs
     body (KernTextV xs)  = null xs


-- | Annotate a Primitive with an @id@ for SVG.
--
-- Note - for @PLabel@ this /pushes/ the id /inside/ the 
-- constructor, for other elements the the id adds an extra layer 
-- of nesting via the SVG group \<g\> tag.
-- 
pushXIdAnno :: String -> Primitive -> Primitive
pushXIdAnno ss (PLabel props (PrimLabel txt _ ctm )) = 
    PLabel props $ PrimLabel txt (Just ss) ctm
 
pushXIdAnno ss prim                                  = PSVG (SvgId ss) prim
