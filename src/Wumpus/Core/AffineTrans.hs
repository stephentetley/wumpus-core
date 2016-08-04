{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}
{-# LANGUAGE UndecidableInstances       #-}

------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.AffineTrans
-- Copyright   :  (c) Stephen Tetley 2009-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Affine transformations.
-- 
-- The common affine transformations represented as type classes -
-- scaling, rotation, translation.
--
-- Internally, when a Picture is composed and transformed, Wumpus
-- only transforms the bounding box - transformations of the 
-- picture content (paths or text labels) are communicated to 
-- PostScript or SVG for final rendering. This is because Wumpus 
-- has no access to the paths that make fonts so cannot transform 
-- them directly.
--
-- Other elements - Vectors, Points, BoundingBoxes and Primtives - 
-- are also instances of the affine classes. However, generally 
-- Wumpus transforms these elements directly rather than 
-- delegating the transformation to PostScript or SVG (the 
-- situation for the Label primitive is more complicated - the 
-- /start/ point is transformed by Wumpus but a matrix 
-- transformation is sent to PostScript to manipulate the opaque 
-- character objects).
--
-- Note - transformations on Primitives are applied to the control 
-- points of the primitive not the /drawing/. A scaled, stroked 
-- path will be drawn with at the standard line width rather than 
-- with a thicker line. Also, text may not render pleasantly after 
-- it has been transformed, PostScript references seem to caution 
-- against transforming text and recommend changing @/scalefont@ 
-- instead of scaling via a transfomation. 
-- 
-- To generate efficient PostScript, Wumpus relies on the matrix
-- representations of the affine transformations being invertible.
-- Do not scale elements by zero!
--
--
-- Design note - the formulation of the affine classes is not 
-- ideal as dealing with units is avoided and the instances for
-- Point2 and Vec2 are only applicable to @DPoint2@ and @DVec2@.
-- Dealing with units is avoided as some useful units 
-- (particulary Em and En) have contextual interterpretations - 
-- i.e. their size is dependent on the current font size - and so 
-- they cannot be accommodated without some monadic context.
-- 
-- For this reason, the naming scheme for the affine classes was
-- changed at revision 0.50.0 to the current \"d\"-prefixed names.
-- This allows higher-level frameworks to define their own 
-- functions or class-methods using the obvious good names 
-- (@rotate@, @scale@ etc.). The derived operations (@rotate30@, 
-- @uniformScale, etc.) have been removed as a higher-level 
-- implementation is expected to re-implement them accounting for 
-- polymorphic units as necessary.
--  
--------------------------------------------------------------------------------

module Wumpus.Core.AffineTrans
  ( 
  -- * Type classes
    Transform(..)
  , Rotate(..)
  , RotateAbout(..)
  , Scale(..)
  , Translate(..)

  -- * Common rotations
  , rotate30
  , rotate30About
  , rotate45
  , rotate45About
  , rotate60
  , rotate60About
  , rotate90
  , rotate90About
  , rotate120
  , rotate120About
  
  -- * Common scalings
  , uniformScale
  , reflectX
  , reflectY

  -- * Translate by a vector
  , translateBy
  
  -- * Reflections in supplied plane rather than about the origin
  , reflectXPlane
  , reflectYPlane   

  ) where

import Wumpus.Core.Geometry



--------------------------------------------------------------------------------
-- Affine transformations 

--
-- Design Note 
--
-- Perhaps the Transform class is not generally useful in the
-- presence of units.
-- 


-- | Apply a matrix transformation directly.
--
class Transform t where
  transform :: u ~ DUnit t => Matrix3'3 u -> t -> t


instance Transform a => Transform (Maybe a) where
  transform = fmap . transform

instance (u ~ DUnit a, u ~ DUnit b, Transform a, Transform b) => 
    Transform (a,b)  where
  transform mtrx (a,b) = (transform mtrx a, transform mtrx b)


instance Num u => Transform (Point2 u) where
  transform ctm = (ctm *#)

instance Num u => Transform (Vec2 u) where
  transform ctm = (ctm *#)


--------------------------------------------------------------------------------

-- | Type class for rotation.
-- 
class Rotate t where
  rotate :: Radian -> t -> t


instance Rotate a => Rotate (Maybe a) where
  rotate = fmap . rotate


instance (Rotate a, Rotate b) => Rotate (a,b)  where
  rotate ang (a,b) = (rotate ang a, rotate ang b)


instance (Real u, Floating u) => Rotate (Point2 u) where
  rotate ang pt = P2 x y 
    where
      v        = pvec zeroPt pt
      (V2 x y) = avec (ang + vdirection v) $ vlength v


instance (Real u, Floating u) => Rotate (Vec2 u) where
  rotate ang v = avec (ang + vdirection v) $ vlength v

--
--

-- | Type class for rotation about a point.
--
-- Note - the point is a @DPoint2@ - i.e. it has PostScript points
-- for x and y-units.
--
class RotateAbout t where
  rotateAbout :: u ~ DUnit t => Radian -> Point2 u -> t -> t


--
-- Note - it seems GHC 7.0.2 at least, would let us define a 
-- RotateAbout instance for @()@, even though it has no valid
-- DUnit instance.
--
-- Still it seems safer to define a nil type with a phantom unit:
--
-- > data UNil u = UNil
-- 
-- This data type is provided by Wumpus-Basic.
-- 


instance RotateAbout a => RotateAbout (Maybe a) where
  rotateAbout ang pt = fmap (rotateAbout ang pt)


instance (u ~ DUnit a, u ~ DUnit b, RotateAbout a, RotateAbout b) => 
    RotateAbout (a,b) where
  rotateAbout ang pt (a,b) = (rotateAbout ang pt a, rotateAbout ang pt b)

instance (Real u, Floating u) => RotateAbout (Point2 u) where
  rotateAbout ang (P2 ox oy) = 
    translate ox oy . rotate ang . translate (-ox) (-oy) 



instance (Real u, Floating u) => RotateAbout (Vec2 u) where
  rotateAbout ang (P2 ox oy) = 
    translate ox oy . rotate ang . translate (-ox) (-oy) 

  
--------------------------------------------------------------------------------
-- Scale

-- | Type class for scaling.
--
class Scale t where
  scale :: Double -> Double -> t -> t


instance Scale a => Scale (Maybe a) where
  scale sx sy = fmap (scale sx sy)

instance (Scale a, Scale b) => Scale (a,b) where
  scale sx sy (a,b) = (scale sx sy a, scale sx sy b)

instance Fractional u => Scale (Point2 u) where
  scale sx sy (P2 x y) = P2 (x * realToFrac sx) (y * realToFrac sy)

instance Fractional u => Scale (Vec2 u) where
  scale sx sy (V2 x y) = V2 (x * realToFrac sx) (y * realToFrac sy)

--------------------------------------------------------------------------------
-- Translate

-- | Type class for translation.
--
class Translate t where
  translate :: u ~ DUnit t => u -> u -> t -> t

instance Translate a => Translate (Maybe a) where
  translate dx dy = fmap (translate dx dy)

instance (u ~ DUnit a, u ~ DUnit b, Translate a, Translate b) => 
    Translate (a,b) where
  translate dx dy (a,b) = (translate dx dy a, translate dx dy b)

instance Num u => Translate (Point2 u) where
  translate dx dy (P2 x y) = P2 (x + dx) (y + dy)

-- | Vectors do not respond to translation.
--
instance Translate (Vec2 u) where
  translate _ _ v0 = v0



-------------------------------------------------------------------------------- 
-- Common rotations


-- | Rotate by 30 degrees about the origin. 
--
rotate30 :: Rotate t => t -> t 
rotate30 = rotate (pi/6) 

-- | Rotate by 30 degrees about the supplied point.
--
rotate30About :: (RotateAbout t, DUnit t ~ u) => Point2 u -> t -> t 
rotate30About = rotateAbout (pi/6)

-- | Rotate by 45 degrees about the origin. 
--
rotate45 :: Rotate t => t -> t 
rotate45 = rotate (pi/4) 

-- | Rotate by 45 degrees about the supplied point.
--
rotate45About :: (RotateAbout t, DUnit t ~ u) => Point2 u -> t -> t 
rotate45About = rotateAbout (pi/4)

-- | Rotate by 60 degrees about the origin. 
--
rotate60 :: Rotate t => t -> t 
rotate60 = rotate (2*pi/3) 

-- | Rotate by 60 degrees about the supplied point.
--
rotate60About :: (RotateAbout t, DUnit t ~ u) => Point2 u -> t -> t 
rotate60About = rotateAbout (2*pi/3)

-- | Rotate by 90 degrees about the origin. 
--
rotate90 :: Rotate t => t -> t 
rotate90 = rotate (pi/2) 

-- | Rotate by 90 degrees about the supplied point.
--
rotate90About :: (RotateAbout t, DUnit t ~ u) => Point2 u -> t -> t 
rotate90About = rotateAbout (pi/2)

-- | Rotate by 120 degrees about the origin. 
--
rotate120 :: Rotate t => t -> t 
rotate120 = rotate (4*pi/3) 

-- | Rotate by 120 degrees about the supplied point.
--
rotate120About :: (RotateAbout t, DUnit t ~ u) => Point2 u -> t -> t 
rotate120About = rotateAbout (4*pi/3)



--------------------------------------------------------------------------------
-- Common scalings

-- | Scale both x and y dimensions by the same amount.
--
uniformScale :: Scale t => Double -> t -> t 
uniformScale a = scale a a 

-- | Reflect in the X-plane about the origin.
--
reflectX :: Scale t => t -> t
reflectX = scale (-1) 1

-- | Reflect in the Y-plane about the origin.
--
reflectY :: Scale t => t -> t
reflectY = scale 1 (-1)

--------------------------------------------------------------------------------
-- Translations

-- | Translate by the x and y components of a vector.
--
translateBy :: (Translate t, DUnit t ~ u) => Vec2 u -> t -> t 
translateBy (V2 x y) = translate x y


--------------------------------------------------------------------------------
-- Translation and scaling

-- | Reflect in the X plane that intersects the supplied point. 
--
reflectXPlane :: (Num u, Scale t, Translate t, u ~ DUnit t) 
              => Point2 u -> t -> t
reflectXPlane (P2 x y) = translate x y . scale (-1) 1 . translate (-x) (-y)

-- | Reflect in the Y plane that intersects the supplied point.
--
reflectYPlane :: (Num u, Scale t, Translate t, u ~ DUnit t) 
              => Point2 u -> t -> t
reflectYPlane (P2 x y) = translate x y . scale 1 (-1) . translate (-x) (-y)
