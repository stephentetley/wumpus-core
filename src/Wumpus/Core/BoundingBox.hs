{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.BoundingBox
-- Copyright   :  (c) Stephen Tetley 2009-2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Bounding box with no notion of \'empty\'.
--
-- Empty pictures cannot be created with Wumpus. This 
-- significantly simplifies the implementation of pictures and 
-- bounding boxes.
--
-- Note - some of the functions exposed by this module are 
-- expected to be pertinent only to Wumpus-Core itself.
-- 
-- 
--------------------------------------------------------------------------------

module Wumpus.Core.BoundingBox 
  ( 
  -- * Types
    BoundingBox(..)
  , DBoundingBox

  -- * Type class
  , Boundary(..)
  
  -- * Constructors
  , boundingBox
  
  -- * Operations
  , destBoundingBox
  , boundaryUnion 
  , traceBoundary
  , retraceBoundary

  , boundaryCorners
  , boundaryCornerList
  , boundaryCenter
  , withinBoundary
  , boundaryWidth
  , boundaryHeight



  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.Geometry
import Wumpus.Core.Utils.FormatCombinators


-- | Bounding box of a picture, path, etc. represented by the 
-- lower-left and upper-right corners.
-- 
-- Wumpus cannot construct empty pictures - so bounding boxes are 
-- spared the obligation to be /empty/. This greatly helps keep 
-- the implementation relatively simple.
-- 
-- BoundingBox operates as a semigroup where @boundaryUnion@ is the
-- addition.
-- 
-- 
data BoundingBox u = BBox 
      { ll_corner :: Point2 u
      , ur_corner :: Point2 u 
      }
  deriving (Show)

type DBoundingBox = BoundingBox Double

type instance DUnit (BoundingBox u) = u

--------------------------------------------------------------------------------
-- instances

instance (Tolerance u, Ord u) => Eq (BoundingBox u) where
  BBox ll0 ur0 == BBox ll1 ur1  = ll0 == ll1 && ur0 == ur1

instance Functor BoundingBox where
  fmap f (BBox p0 p1) = BBox (fmap f p0) (fmap f p1)

instance Format u => Format (BoundingBox u) where
  format (BBox p0 p1) = parens (text "BBox" <+> text "ll=" >< format p0 
                                            <+> text "ur=" >< format p1) 


--------------------------------------------------------------------------------
-- Transform...

-- | Helper for transformation.
--
pointTransform :: (Num u, Ord u) 
               => (Point2 u -> Point2 u) -> BoundingBox u -> BoundingBox u
pointTransform fn bb = 
    traceBoundary $ map fn $ [bl,br,tr,tl]
  where 
    (bl,br,tr,tl) = boundaryCorners bb



instance (Num u, Ord u) => Transform (BoundingBox u) where
  transform mtrx = pointTransform  (mtrx *#)

instance (Real u, Floating u, Ord u) => Rotate (BoundingBox u) where
  rotate theta = pointTransform (rotate theta)

instance (Real u, Floating u, Ord u) => RotateAbout (BoundingBox u) where
  rotateAbout theta pt = pointTransform (rotateAbout theta pt)

instance (Fractional u, Ord u) => Scale (BoundingBox u) where
  scale sx sy = pointTransform (scale sx sy)

instance (Num u, Ord u) => Translate (BoundingBox u) where
  translate dx dy = pointTransform (translate dx dy)

--------------------------------------------------------------------------------
-- Boundary class

-- | Type class extracting the bounding box of an object - 
-- Picture, Path etc.
--
class Boundary t where
  boundary :: u ~ DUnit t => t -> BoundingBox u 


instance Boundary (BoundingBox u) where
  boundary = id

--------------------------------------------------------------------------------

-- | 'boundingBox' : @lower_left_corner * upper_right_corner -> BoundingBox@
--
-- Contruct a bounding box, vis the BBox constructor with range 
-- checking on the corner points.
--
-- 'boundingBox' throws an error if the width or height of the 
-- constructed bounding box is negative.
--
boundingBox :: Ord u => Point2 u -> Point2 u -> BoundingBox u
boundingBox ll@(P2 x0 y0) ur@(P2 x1 y1) 
    | x0 <= x1 && y0 <= y1 = BBox ll ur 
    | otherwise            = error "Wumpus.Core.boundingBox - malformed."


-- | 'destBoundingBox' : @ bbox -> (lower_left_x, lower_left_y, 
--      upper_right_x, upper_right_y)@
--
-- Destructor for BoundingBox, assembles a four-tuple of the x 
-- and y values of the corner points.
-- 
-- Arguably this is easier to pattern match upon, as it removes a 
-- layer of nesting.
--
destBoundingBox :: BoundingBox u -> (u,u,u,u)
destBoundingBox (BBox (P2 llx lly) (P2 urx ury)) = (llx, lly, urx, ury) 


-- | The union of two bounding boxes. 
--
boundaryUnion :: Ord u => BoundingBox u -> BoundingBox u -> BoundingBox u
BBox ll ur `boundaryUnion` BBox ll' ur' = BBox (minPt ll ll') (maxPt ur ur')


-- | 'traceBoundary' : @ points -> BoundingBox @
--
-- Trace a list of points, retuning the BoundingBox of their
-- boundary.
--
-- \*\* WARNING \*\* - 'trace' throws a run-time error when 
-- supplied with the empty list.
--
traceBoundary :: (Num u, Ord u) => [Point2 u] -> BoundingBox u
traceBoundary (p:ps) = 
    uncurry BBox $ foldr (\z (a,b) -> (minPt z a, maxPt z b) ) (p,p) ps
traceBoundary []     = error $ "BoundingBox.trace called in empty list"


-- | Perform the supplied transformation on the four corners of 
-- the bounding box. Trace the new corners to calculate the 
-- resulting bounding box.
-- 
-- NOTE - this helper function is used within Wumpus-Core to 
-- re-calculate a bounding box after a rotation for example. It is 
-- probably useful only to Wumpus-Core.
--
retraceBoundary :: (Num u, Ord u) 
        => (Point2 u -> Point2 u) -> BoundingBox u -> BoundingBox u
retraceBoundary f = traceBoundary . map f . fromCorners . boundaryCorners
  where
    fromCorners (bl,br,tr,tl) = [bl,br,tr,tl]


-- | 'boundaryCorners' : @bbox -> (bottom_left, bottom_right,
--      top_right, top_left)@
-- 
-- Generate all the corners of a bounding box, counter-clock 
-- wise from the bottom left, i.e. @(bl, br, tr, tl)@.
--
boundaryCorners :: BoundingBox u -> (Point2 u, Point2 u, Point2 u, Point2 u)
boundaryCorners (BBox bl@(P2 x0 y0) tr@(P2 x1 y1)) = (bl, br, tr, tl) 
  where
    br = P2 x1 y0
    tl = P2 x0 y1


-- | 'boundaryCornerList' : @bbox -> [bottom_left, bottom_right,
--      top_right, top_left]@
-- 
-- Generate all the corners of a bounding box, counter-clock 
-- wise from the bottom left, i.e. @[bl, br, tr, tl]@. 
--
-- This is a list version of 'boundaryCorners' which is sometimes 
-- more convenient. For instance, to create a vertex path it is 
-- more direct to use this list rather than build one from the 
-- 4-tuple returned by 'boundaryCorners'.
--
boundaryCornerList :: BoundingBox u -> [Point2 u]
boundaryCornerList (BBox bl@(P2 x0 y0) tr@(P2 x1 y1)) = [bl, br, tr, tl]
  where
    br = P2 x1 y0
    tl = P2 x0 y1


-- | 'boundaryCenter' : @bbox -> Point@
-- 
-- Return the center of a bounding box.
--
boundaryCenter :: Fractional u => BoundingBox u -> Point2 u
boundaryCenter (BBox (P2 x0 y0) (P2 x1 y1)) = P2 x y 
  where
    x = x0 + (0.5*(x1-x0))
    y = y0 + (0.5*(y1-y0))


-- | 'withinBoundary' : @ point * bbox -> Bool @
-- 
-- Within test - is the supplied point within the bounding box?
--
withinBoundary :: (Tolerance u, Ord u) => Point2 u -> BoundingBox u -> Bool
withinBoundary p (BBox ll ur) = (minPt p ll) == ll && (maxPt p ur) == ur


-- | 'boundaryWidth' : @ bbox -> Width @
--
-- Extract the width of a bounding box.
--
boundaryWidth :: Num u => BoundingBox u -> u
boundaryWidth (BBox (P2 xmin _) (P2 xmax _)) = xmax - xmin


-- | 'boundaryHeight' : @ bbox -> Height @
--
-- Extract the height of a bounding box.
--
boundaryHeight :: Num u => BoundingBox u -> u
boundaryHeight (BBox (P2 _ ymin) (P2 _ ymax)) = ymax - ymin




