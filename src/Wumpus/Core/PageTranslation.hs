{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PageTranslation
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Core page translation for SVG.
--
-- Note - initially an optimized translate was planned - smaller 
-- SVG code size and less use of /rectifying/ transformations for 
-- text. However working out a translation scheme proved to be 
-- tricky and Wumpus is stuck with the /trivialTransformation/.
--
--------------------------------------------------------------------------------


module Wumpus.Core.PageTranslation
  ( 
    svgPageTranslation

  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.PictureInternal
import Wumpus.Core.TrafoInternal

--------------------------------------------------------------------------------
-- trivial translation

-- Rescale the entire image (1,-1), rescale the ellipse and 
-- label primitives as they are encoutered (1,-1). No need to 
-- worry about scaling the BoundingBox
--




svgPageTranslation :: Picture -> Picture
svgPageTranslation pic = scale 1 (-1) (trivPic pic)



trivPic :: Picture -> Picture
trivPic (Leaf lc ones)      = Leaf lc (fmap trivPrim ones)
trivPic (Picture lc ones)   = Picture lc (fmap trivPic ones)


-- | Path is unchanged because it is drawn directly in the output
-- and thus doesn\'t need a rectifying transformation.
--
trivPrim :: Primitive -> Primitive
trivPrim (PPath a pp)     = PPath a pp
trivPrim (PLabel a lbl)   = PLabel a (trivLabel lbl)
trivPrim (PEllipse a ell) = PEllipse a (trivEllipse ell)
trivPrim (PContext a chi) = PContext a (trivPrim chi)
trivPrim (PSVG a chi)     = PSVG a (trivPrim chi)
trivPrim (PGroup ones)    = PGroup $ fmap trivPrim ones
trivPrim (PClip pp chi)   = PClip pp (trivPrim chi)



trivLabel :: PrimLabel -> PrimLabel
trivLabel (PrimLabel txt opt_id ctm) = 
    PrimLabel txt opt_id (trivPrimCTM ctm)

trivEllipse :: PrimEllipse -> PrimEllipse
trivEllipse (PrimEllipse hw hh ctm) = PrimEllipse hw hh (trivPrimCTM ctm)

-- Negate the y scaling to flip the image.
--
trivPrimCTM :: PrimCTM -> PrimCTM
trivPrimCTM (PrimCTM dx dy sx sy theta) = PrimCTM dx dy sx (negate sy) theta

