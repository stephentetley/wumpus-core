{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.TrafoInternal
-- Copyright   :  (c) Stephen Tetley 2010-2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable 
-- Portability :  GHC
--
-- Transformations on (Path) Primtives in Wumpus-Core are 
-- performed on the control points rather than transmitted to 
-- PostScript. 
-- 
-- However because text labels are opaque to Wumpus, the corner
-- start point is manipulated in Wumpus, but transformations on 
-- the actual text are communicated to PostScript as matrix 
-- transformations. 
-- 
--------------------------------------------------------------------------------


module Wumpus.Core.TrafoInternal
  (

  -- * Types
    PrimCTM(..)

  , AffineTrafo(..) 

  -- * CTM operations
  , identityCTM
  , makeThetaCTM
  , makeTranslCTM
  , startPointCTM

  , translateCTM
  , scaleCTM
  , rotateCTM
  , rotateAboutCTM
  , matrixRepCTM
  , unCTM

  -- * AffineTrafo operations
  , concatTrafos
  , matrixRepr
  
  ) where


import Wumpus.Core.AffineTrans
import Wumpus.Core.Geometry
import Wumpus.Core.Utils.Common ( dtruncFmt )
import Wumpus.Core.Utils.FormatCombinators

-- Note - PrimCTM can be specialized to Double.

-- Primitives support affine transformations.
--
-- The control points of a path, baseline-left of text or center
-- of ellipse are transformed as points; scaling and rotation 
-- or text and ellipses are transmitted to PostScript and SVG.
-- 
-- Note - line thickness of a stroked path will not be scaled.
--
data PrimCTM = PrimCTM 
      { ctm_trans_x     :: !Double
      , ctm_trans_y     :: !Double
      , ctm_scale_x     :: !Double
      , ctm_scale_y     :: !Double
      , ctm_rotation    :: !Radian 
      }
  deriving (Eq,Show)


type instance DUnit PrimCTM = Double


-- | For Pictures - Affine transformations are represented as 
-- /syntax/ so they can be manipulated easily.
--
data AffineTrafo = Matrix (Matrix3'3 Double)
                 | Rotate Radian
                 | RotAbout Radian (Point2 Double)
                 | Scale Double Double
                 | Translate Double Double
  deriving (Eq,Show)                 

type instance DUnit AffineTrafo = Double

--------------------------------------------------------------------------------
-- instances

instance Format PrimCTM where
  format (PrimCTM dx dy sx sy ang) = 
      parens (text "CTM" <+> text "dx =" >< dtruncFmt dx
                         <+> text "dy =" >< dtruncFmt dy
                         <+> text "sx =" >< dtruncFmt sx 
                         <+> text "sy =" >< dtruncFmt sy 
                         <+> text "ang=" >< format ang  )



--------------------------------------------------------------------------------
-- Manipulating the PrimCTM

identityCTM :: PrimCTM
identityCTM = PrimCTM { ctm_trans_x  = 0
                      , ctm_trans_y  = 0
                      , ctm_scale_x  = 1
                      , ctm_scale_y  = 1
                      , ctm_rotation = 0   }


makeThetaCTM :: Double -> Double -> Radian -> PrimCTM
makeThetaCTM dx dy ang = PrimCTM { ctm_trans_x  = dx
                                 , ctm_trans_y  = dy
                                 , ctm_scale_x  = 1
                                 , ctm_scale_y  = 1
                                 , ctm_rotation = ang }


makeTranslCTM :: Double -> Double -> PrimCTM
makeTranslCTM dx dy = PrimCTM { ctm_trans_x  = dx
                              , ctm_trans_y  = dy
                              , ctm_scale_x  = 1
                              , ctm_scale_y  = 1
                              , ctm_rotation = 0 }


startPointCTM :: DPoint2 -> PrimCTM
startPointCTM (P2 x y) = PrimCTM { ctm_trans_x  = x
                                 , ctm_trans_y  = y
                                 , ctm_scale_x  = 1
                                 , ctm_scale_y  = 1
                                 , ctm_rotation = 0 }




translateCTM :: Double -> Double -> PrimCTM -> PrimCTM
translateCTM x1 y1 (PrimCTM dx dy sx sy ang) = 
    PrimCTM (x1+dx) (y1+dy) sx sy ang




-- Note - the matrix is not used entirely conventionally.
--
-- It is expected that the point is extracted from the matrix, so
-- scales and rotations operate on the point coordinates as well
-- as the scale and rotation components. 
--

-- | Scale the CTM.
--
scaleCTM :: Double -> Double -> PrimCTM -> PrimCTM
scaleCTM x1 y1 (PrimCTM dx dy sx sy ang) = 
    let P2 x y = scale x1 y1 (P2 dx dy) 
    in PrimCTM x y (x1*sx) (y1*sy) ang

-- | Rotate the CTM.
--
rotateCTM :: Radian -> PrimCTM -> PrimCTM
rotateCTM theta (PrimCTM dx dy sx sy ang) = 
    let P2 x y = rotate theta (P2 dx dy) 
    in PrimCTM x y sx sy (circularModulo $ theta+ang)

-- | RotateAbout the CTM.
--
rotateAboutCTM :: Radian -> DPoint2 -> PrimCTM -> PrimCTM
rotateAboutCTM theta pt (PrimCTM dx dy sx sy ang) = 
    let P2 x y = rotateAbout theta pt (P2 dx dy)
    in PrimCTM x y sx sy (circularModulo $ theta+ang)


-- Note - the order of combining a translation (i.e. the location 
-- of a point) and the CTM is crucial as matrix multiplication is 
-- not commutative.
--
-- This function encapsulates the correct order (or does it? - 
-- some of the demos are not working properly...).
--
matrixRepCTM :: PrimCTM -> Matrix3'3 Double
matrixRepCTM (PrimCTM dx dy sx sy ang) = 
    translationMatrix dx dy * rotationMatrix (circularModulo ang) 
                            * scalingMatrix sx sy


-- | Destructor for the CTM extracts the /start point/ and a
-- /residual/ CTM. 
-- 
-- If the residual CTM is the identity CTM, the SVG or PostScript
-- output can be optimized.
-- 
unCTM :: PrimCTM -> (DPoint2, PrimCTM)
unCTM (PrimCTM dx dy sx sy ang) = (P2 dx dy, PrimCTM 0 0 sx sy ang)


--------------------------------------------------------------------------------
-- AffineTrafo operations



concatTrafos :: [AffineTrafo] -> Matrix3'3 Double
concatTrafos = foldr (\e ac -> matrixRepr e * ac) identityMatrix

matrixRepr :: AffineTrafo -> Matrix3'3 Double
matrixRepr (Matrix mtrx)        = mtrx
matrixRepr (Rotate theta)       = rotationMatrix theta
matrixRepr (RotAbout theta pt)  = originatedRotationMatrix theta pt
matrixRepr (Scale sx sy)        = scalingMatrix sx sy 
matrixRepr (Translate dx dy)    = translationMatrix dx dy

