{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Colour
-- Copyright   :  (c) Stephen Tetley 2009-2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Colour represented as RGB with each component in the range 
-- [0..255].
-- 
-- Note - the predefined colours are hidden when importing the
-- /top-level/ shim module @Wumpus.Core@, import 
-- @Wumpus.Core.Colour@ directly to use them.
--
-- PostScript has no support for RGB-alpha and hence does not
-- support transparency. Thus Wumpus in turn cannot support 
-- transparency.
--
--------------------------------------------------------------------------------


module Wumpus.Core.Colour
  ( 

  -- * RGB colour type  
    RGBi(..)

  -- * Predefined colours
  , black
  , white
  , red
  , green
  , blue
  , yellow
  , cyan
  , magenta
    
  ) where

import Wumpus.Core.Utils.FormatCombinators

import Data.Word

-- | Colours levels are in the range [0..255]
-- 
-- Note - this is the format used by SVG, whereas PostScript uses 
-- [0..1]. 
--
-- It is more efficient to prefer SVG here.
--
data RGBi = RGBi !Word8 !Word8 !Word8
  deriving (Eq,Ord,Show)


--------------------------------------------------------------------------------
-- instances

instance Format RGBi where
  format (RGBi 0   0   0)    = text "*black*"
  format (RGBi 255 255 255)  = text "*white*"
  format (RGBi r   g   b)    = integral r >< comma >< integral g 
                                          >< comma >< integral b


--------------------------------------------------------------------------------


-- Some colours

-- There will be name clashes with the X11Colours / SVGColours.

-- | Black - 0, 0, 0.
--
black           :: RGBi
black           = RGBi 0 0 0

-- | White - 255, 255, 255.
--
white           :: RGBi
white           = RGBi 255 255 255

-- | Red - 255, 0, 0.
--
red             :: RGBi
red             = RGBi 255 0 0

-- | Green - 0, 255, 0.
--
green           :: RGBi 
green           = RGBi 0 255 0

-- | Blue - 0, 0, 255.
--
blue            :: RGBi
blue            = RGBi 0 0 255

-- | Yellow - 255, 255, 0.
--
yellow          :: RGBi
yellow          = RGBi 255 255 0

-- | Cyan - 0, 255, 255.
--
cyan            :: RGBi
cyan            = RGBi 0 255 255

-- | Magenta - 255, 0, 255.
--
magenta         :: RGBi
magenta         = RGBi 255 0 255
