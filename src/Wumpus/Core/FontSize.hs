{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.FontSize
-- Copyright   :  (c) Stephen Tetley 2009-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Approximate glyph size calculations for Label\'s and their 
-- bounding boxes.
-- 
-- Calculations are based on metrics derived from the Courier 
-- font. As Courier is a monospaced font, applying these metrics
-- to other font families will usually produce over-estimates
-- (bounding boxes will be longer than the true visual length
-- of the text). Furthermore, even italic or bold Courier will 
-- have different metrics.
-- 
-- This is a deficiency of Wumpus, and limits its text handling
-- capabilities - for example, text cannot be reliably centered 
-- or right aligned as its true length is not known. However, more 
-- powerful alternatives would need access to the metrics embedded 
-- within font files. This would require a font loader and add 
-- significant implementation complexity.
-- 
--------------------------------------------------------------------------------

module Wumpus.Core.FontSize
  ( 
  
  -- * Type synonyms
    FontSize
  , CharCount
  , AfmUnit
  , afmUnit
  , afmValue

  -- * Scaling values derived from Courier
  , mono_width
  , mono_cap_height
  , mono_x_height
  , mono_descender
  , mono_ascender
  , mono_left_margin
  , mono_right_margin

  -- * Courier metrics
  , charWidth
  , textWidth
  , capHeight
  , xcharHeight
  , totalCharHeight
  , ascenderHeight
  , descenderDepth

  -- * Size calculation
  , textBounds
  , textBoundsEsc
  , charCount

  ) where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Geometry
import Wumpus.Core.Text.Base


type CharCount = Int
type FontSize = Int


-- | Wrapped Double representing 1\/1000 of the scale factor
-- (Point size) of a font. AFM files encode all measurements 
-- as these units. 
-- 
newtype AfmUnit = AfmUnit { getAfmUnit :: Double } 
  deriving (Eq,Ord,Num,Floating,Fractional,Real,RealFrac,RealFloat)


instance Show AfmUnit where
  showsPrec p d = showsPrec p (getAfmUnit d)

instance Tolerance AfmUnit where
  eq_tolerance     = 0.001
  length_tolerance = 0.1


-- | Flipped version of 'afmValue'.
--
afmValueSZ :: AfmUnit -> FontSize -> Double
afmValueSZ = flip afmValue


-- | Compute the size of a measurement in PostScript points 
-- scaling the Afm unit size by the point size of the font.
--
afmValue :: FontSize -> AfmUnit -> Double
afmValue sz u = realToFrac u * (fromIntegral sz) / 1000


-- | Compute the size of a measurement in Afm units scaled by the
-- point size of the font.
--
afmUnit :: FontSize -> Double -> AfmUnit
afmUnit sz u = 1000.0 * (realToFrac u) / (fromIntegral sz) 



-- NOTE - I\'ve largely tried to follow the terminoloy from 
-- Edward Tufte\'s /Visual Explantions/, page 99.
--


-- | The ratio of width to point size of a letter in Courier.
--
-- > mono_width = 600
--
mono_width :: AfmUnit
mono_width = 600

-- | The ratio of cap height to point size of a letter in Courier.
--
-- > mono_cap_height = 562
-- 
mono_cap_height :: AfmUnit 
mono_cap_height = 562




-- | The ratio of x height to point size of a letter in Courier. 
--
-- This is also known as the \"body height\".
--
-- > mono_x_height = 426
-- 
mono_x_height :: AfmUnit
mono_x_height = 426


-- | The ratio of descender depth to point size of a letter in 
-- Courier.
-- 
-- > mono_descender = -157
-- 
mono_descender :: AfmUnit
mono_descender = (-157)


-- | The ratio of ascender to point size of a letter in Courier.
-- 
-- > mono_ascender = 629
-- 
mono_ascender :: AfmUnit
mono_ascender = 629


-- | The distance from baseline to max height as a ratio to point 
-- size for Courier.
-- 
-- > mono_max_height = 805
-- 
mono_max_height :: AfmUnit 
mono_max_height = 805


-- | The distance from baseline to max depth as a ratio to point 
-- size for Courier.
-- 
-- > max_depth = -250
-- 
mono_max_depth :: AfmUnit 
mono_max_depth = (-250)


-- | The left margin for the bounding box of printed text as a 
-- ratio to point size for Courier.
-- 
-- > mono_left_margin = -46
-- 
mono_left_margin :: AfmUnit 
mono_left_margin = (-46)


-- | The right margin for the bounding box of printed text as a 
-- ratio to point size for Courier.
-- 
-- > mono_right_margin = 50
-- 
mono_right_margin :: AfmUnit 
mono_right_margin = 50


-- | Approximate the width of a monospace character using 
-- metrics derived from the Courier font.
--
charWidth :: FontSize -> Double
charWidth = afmValueSZ mono_width



-- | 'textWidth' : @ font_size * char_count -> PtSize @
--
-- Text width at the supplied font_size. It is expected that the
-- @char_ount@ has been calculated with the @charCount@ function.
--
-- NOTE - this does not account for any left and right margins 
-- around the printed text.
--
textWidth :: FontSize -> CharCount -> Double
textWidth _  n | n <= 0 = 0
textWidth sz n          = fromIntegral n * charWidth sz


-- | Height of capitals e.g. \'A\' using metrics derived 
-- the Courier monospaced font.
--
capHeight :: FontSize -> Double
capHeight = fromIntegral


-- | Height of the lower-case char \'x\' using metrics derived 
-- the Courier monospaced font.
--
xcharHeight :: FontSize -> Double
xcharHeight = afmValueSZ mono_x_height

-- | The total height span of the glyph bounding box for the 
-- Courier monospaced font.
--
totalCharHeight :: FontSize -> Double
totalCharHeight sz =  
    afmValueSZ mono_max_height sz + negate (afmValueSZ mono_max_depth sz)
  

-- | Ascender height for font size @sz@ using metrics from the 
-- Courier monospaced font.
-- 
ascenderHeight :: FontSize -> Double
ascenderHeight = afmValueSZ mono_ascender



-- | Descender depth for font size @sz@ using metrics from the 
-- Courier monospaced font.
-- 
descenderDepth :: FontSize -> Double
descenderDepth = afmValueSZ mono_descender


-- | 'textBounds' : @ font_size * baseline_left * text -> BBox @
--
-- Find the bounding box for the character count at the 
-- supplied font-size.
-- 
-- The supplied point represents the baseline left corner of the 
-- a regular upper-case letter (that is without descenders).
-- The bounding box adds a margin around all sides of the text.
--  
-- The metrics used are derived from Courier - a monospaced font.
-- For proportional fonts the calculated bounding box will 
-- usually be too long.
--
textBounds :: FontSize -> DPoint2 -> String -> BoundingBox Double
textBounds sz pt ss = textBoundsBody sz pt (charCount ss) 


-- | 'textBoundsEsc' : @ font_size * baseline_left * escaped_text -> BBox @
-- 
--  Version of textBounds for already escaped text.
--
textBoundsEsc :: FontSize -> DPoint2 -> EscapedText -> BoundingBox Double
textBoundsEsc sz pt esc = textBoundsBody sz pt (textLength esc) 


textBoundsBody :: FontSize -> DPoint2 -> Int -> BoundingBox Double
textBoundsBody sz (P2 x y) len = boundingBox ll ur
  where
    w           = textWidth  sz len
    left_m      = afmValueSZ mono_left_margin  sz
    right_m     = afmValueSZ mono_right_margin sz
    max_depth   = afmValueSZ mono_max_depth    sz
    max_height  = afmValueSZ mono_max_height   sz
    ll          = P2 (x + left_m)      (y + max_depth)
    ur          = P2 (x + w + right_m) (y + max_height)




-- | 'charCount' : @ string -> CharCount @
--
-- Count the characters in the supplied string, escaping the 
-- string as necessary.
--
-- Escapes count as one character - for instance, the length of 
-- this string:
--
-- > abcd&#egrave;f
--
-- ... is 6.
-- 
charCount :: String -> CharCount
charCount = outstep 0 
  where
    outstep n ('&':'#':xs)  = instep n xs
    outstep n (_:xs)        = outstep (n+1) xs
    outstep n []            = n
    
    instep  n (';':xs)      = outstep (n+1) xs
    instep  n (_:xs)        = instep  n xs
    instep  n []            = n                

-- Note - the last case of instep indicates a malformed string, 
-- but there is nothing that can be done. Promoting to Maybe or 
-- Either would complicated the interface and doesn\'t seem worth
-- it. 