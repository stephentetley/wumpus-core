{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core
-- Copyright   :  (c) Stephen Tetley 2009-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable 
-- Portability :  GHC
--
-- Common interface to Wumpus.Core.
--
-- This is a /shim/ module re-exporting types and functions from
-- the exposed Wumpus-Core modules. In most cases, importing just 
-- this module should be sufficient to use Wumpus-Core. 
--
-- Named colours ( black, white etc.) are hidden from the module
-- "Wumpus.Core.Colour" to avoid collisions with modules that
-- define colour sets (e.g. all the SVG colours). If named 
-- colours are needed, "Wumpus.Core.Colour" can be imported 
-- directly.
-- 
-- Courier metrics (charWidth, textHeight, etc.) are hidden from 
-- the module "Wumpus.Core.FontSize". As these metrics are 
-- somewhat approximate, it is expected that if higher-level 
-- software needs these functionality it should define its own
-- implementations. However the functions may be convenient and if 
-- they are needed, "Wumpus.Core.FontSize" can be imported 
-- directly.
--
--------------------------------------------------------------------------------


module Wumpus.Core
  (
    module Wumpus.Core.AffineTrans 
  , module Wumpus.Core.BoundingBox
  , module Wumpus.Core.Colour
  , module Wumpus.Core.FontSize
  , module Wumpus.Core.Geometry
  , module Wumpus.Core.GraphicProps
  , module Wumpus.Core.OutputPostScript
  , module Wumpus.Core.OutputSVG
  , module Wumpus.Core.Picture
  , module Wumpus.Core.Text.Base
  , module Wumpus.Core.VersionNumber
  , module Wumpus.Core.WumpusTypes

  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.BoundingBox
import Wumpus.Core.Colour hiding 
    ( black, white, red, green, blue, yellow, cyan, magenta )

import Wumpus.Core.FontSize hiding 
    ( charWidth, textWidth, capHeight, xcharHeight, totalCharHeight
    , ascenderHeight, descenderDepth ) 

import Wumpus.Core.Geometry
import Wumpus.Core.GraphicProps
import Wumpus.Core.OutputPostScript
import Wumpus.Core.OutputSVG
import Wumpus.Core.Picture
import Wumpus.Core.Text.Base
import Wumpus.Core.VersionNumber
import Wumpus.Core.WumpusTypes

