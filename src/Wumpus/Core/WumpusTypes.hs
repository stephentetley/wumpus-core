{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.WumpusTypes
-- Copyright   :  (c) Stephen Tetley 2009-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable 
-- Portability :  GHC
--
-- This module re-exports types and functions from 
-- "Wumpus.Core.PictureInternal" but makes them opaque. 
-- Contructors are provided by "Wumpus.Core.Picture".
--
-- Note - whilst the Picture types support the Eq class the 
-- implementations are not efficient, e.g. In the case of 
-- Primitive, one of the constructors has to unwind a functional 
-- \Hughes list\. The Eq instance is considered a legacy 
-- /feature/ (or burden) as the types have expanded.
-- 
--------------------------------------------------------------------------------


-- Having this module just re-exporting types should make 
-- the Haddock documentation more cohesive. Modules
-- in wumpus-core should not use this module.
-- 
-- Note - XLink only exports the NoLink constructors.

module Wumpus.Core.WumpusTypes
  (

  -- * Picture types
    Picture
  , FontCtx
  , Primitive
  , XLink
  , SvgAttr
  , PrimPath
  , PrimPathSegment
  , AbsPathSegment
  , PrimLabel
  , KerningChar

  , Format(..)
  , stringformat


  ) where


import Wumpus.Core.PictureInternal
import Wumpus.Core.Utils.FormatCombinators ( Format(..), text, Doc )




-- | 'stringformat' : String -> Doc
--
-- The format combinators are not exported by Wumpus-Core, 
-- however for debugging unit types might need to be made 
-- instances of the 'Format' class.
-- 
-- To define Format instances render the unit type to a String
-- then use 'stringformat', e.g:
--
-- >
-- > instance Format Pica where
-- >   format a = stringformat (show a)
-- >
--
stringformat :: String -> Doc
stringformat = text
