{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Text.Base
-- Copyright   :  (c) Stephen Tetley 2010-2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Extended character code handling.
-- 
-- Wumpus uses an escaping style derived from SVG to embed 
-- character codes and PostScript glyph names in regular strings. 
--
-- > "regular ascii text &#38; more ascii text"
--  
-- i.e. character codes are delimited by @&\#@ on the 
-- left and @;@ on the right.
--
-- Glyph names are delimited by @&@ on the left and @;@ on the 
-- right.
--
-- > "regular ascii text &ampersand; more ascii text"
--
-- Note that glyph names \*\* /should always/ \*\* correspond to
-- PostScript glyph names not SVG / HTML glyph names.
--
-- In Wumpus both glyph names and character codes can
-- be embedded in strings - (e.g. @&\egrave;@ or @&\#232;@) 
-- although glyph names are preferred for PostScript (see below).
--
-- Character codes can be also be expressed as octal or 
-- hexadecimal numbers:
--
-- > myst&#0o350;re
--
-- > myst&#0xE8;re
--
-- In the generated PostScript, Wumpus uses the character name, 
-- e.g.:  
--
-- > (myst) show /egrave glyphshow (re) show
-- 
-- The generated SVG uses the numeric code, e.g.: 
--
-- > myst&#232;re
--
-- Unless you are generating only SVG, you should favour glyph 
-- names rather than code points as they are unambiguously 
-- interpreted by Wumpus. Character codes are context-dependent
-- on the encoding of the font used to render the text. 
-- /Standard/ fonts (e.g. Helvetica, Times, Courier) use the 
-- Standard Encoding is which has some differences to the 
-- common Latin1 character set. 
--
-- Unfortunately if a glyph is not present in a font it cannot 
-- be rendered in PostScript. Wumpus-Core is oblivious to the 
-- contents of fonts, it does not warn about missing glyphs or 
-- attempt to substitute them.
-- 
--------------------------------------------------------------------------------


module Wumpus.Core.Text.Base
  ( 

    EscapedText
  , EscapedChar(..)
  , EncodingVector

  , escapeString
  , wrapEscChar
  , destrEscapedText
  , textLength  

  ) where

import Wumpus.Core.Utils.FormatCombinators
import Wumpus.Core.Utils.HList

import Data.Char
import qualified Data.IntMap as IntMap
import Data.Monoid

-- | Internal string representation for Wumpus-Core.
-- 
-- 'EscapedText' is a list of characters, where each character 
-- may be either a regular character, an integer representing a 
-- Unicode code-point or a PostScript glyph name.
-- 
newtype EscapedText = EscapedText { getEscapedText :: H EscapedChar }




-- | Internal character representation for Wumpus-Core.
-- 
-- An 'EscapedChar' may be either a regular character, an integer
-- representing a Unicode code-point or a PostScript glyph
-- name.
-- 
-- PostScript glyph names are generally made up only of chars
-- @[a-zA-Z]@.
-- 
data EscapedChar = CharLiteral Char
                 | CharEscInt  Int
                 | CharEscName String
  deriving (Eq,Show)


-- | 'EncodingVecor' - a map from code point to PostScript glyph
-- name.
-- 
type EncodingVector = IntMap.IntMap String


--------------------------------------------------------------------------------

escapedTextList :: EscapedText -> [EscapedChar]
escapedTextList = toListH . getEscapedText

instance Eq EscapedText where
  (==) a b = escapedTextList a == escapedTextList b

instance Show EscapedText where
  show = show . escapedTextList
  

instance Format EscapedText where
  format = hcat . map format . escapedTextList


instance Format EscapedChar where
  format (CharLiteral c) = char c
  format (CharEscInt i)  = text "&#" >< int i  >< semicolon
  format (CharEscName s) = text "&"  >< text s >< semicolon


instance Monoid EscapedText where
  mempty        = EscapedText emptyH
  a `mappend` b = EscapedText $ getEscapedText a `appendH` getEscapedText b

--------------------------------------------------------------------------------




-- | 'escapeString' input is regular text and escaped glyph names 
-- or decimal character codes. Escaping in the input string should 
-- follow the SVG convention - the escape sequence starts with 
-- @&@ (ampresand) for glyph names or @&#@ (ampersand hash) for
-- char codes and ends with @;@ (semicolon).
--
-- Escaped characters are output to PostScript as their respective
-- glyph names:
--
-- > /egrave glyphshow
--
-- Escaped chararacters are output to SVG as an escaped decimal, 
-- e.g.:
--
-- > &#232;
--
-- Note - for SVG output, Wumpus automatically escapes characters 
-- where the char code is above 128. This is the convention used 
-- by the @Text.XHtml@ library.
--
escapeString :: String -> EscapedText
escapeString = EscapedText . lexer

-- | Build an 'EscapedText' from a single 'EscChar'.
--
wrapEscChar :: EscapedChar -> EscapedText
wrapEscChar ec = EscapedText $ wrapH ec

-- | /Destructor/ for 'EscapedText'.
--
destrEscapedText :: ([EscapedChar] -> a) -> EscapedText -> a
destrEscapedText f = f . escapedTextList


-- | Get the character count of an 'EscapedText' string.
--
textLength :: EscapedText -> Int
textLength = destrEscapedText length

--
-- Design note.
--
-- There is a fair argument that the lexer function should be 
-- supplied with an encoding table and the actual encoding is 
-- perfomed here rather than just escaping...
--

 
lexer :: String -> H EscapedChar
lexer []            = emptyH
lexer ('&':'#':cs)  = escNumStart cs
lexer ('&':cs)      = escName cs
lexer (c:cs)        = CharLiteral c `consH` lexer cs

-- Input is malformed if this reaches the @rest@ case.
-- 
escNumStart :: String -> H EscapedChar
escNumStart ('0':'o':cs)           = escOct cs
escNumStart ('0':'O':cs)           = escOct cs
escNumStart ('0':'x':cs)           = escHex cs
escNumStart ('0':'X':cs)           = escHex cs
escNumStart (c:cs) | isDigit c     = escDec (digitToInt c) cs
escNumStart rest                   = chompToSemi rest      

escName :: String -> H EscapedChar
escName (c:cs) = let (ss,rest) = span isAlphaNum cs 
                 in specialEscape (c:ss) `consH` chompToSemi rest
escName []     = emptyH


-- | One digit consumed already...
--
escDec :: Int -> String -> H EscapedChar
escDec n (c:cs) | isDigit c = escDec (n*10 + digitToInt c) cs
escDec n cs     | n > 0     = CharEscInt n `consH` chompToSemi cs
                | otherwise = chompToSemi cs

escHex :: String -> H EscapedChar
escHex = step 0
  where
    step n (c:cs) | isHexDigit c = step (n*16 + digitToInt c) cs
    step n cs     | n > 0        = CharEscInt n `consH` chompToSemi cs
                  | otherwise    = chompToSemi cs 


escOct :: String -> H EscapedChar
escOct = step 0
  where
    step n (c:cs) | isHexDigit c = step (n*8 + digitToInt c) cs
    step n cs     | n > 0        = CharEscInt n `consH` chompToSemi cs
                  | otherwise    = chompToSemi cs 



-- The last two conditions both indicate ill-formed input, but it
-- is /best/ if the lexer does not throw errors.
-- 
chompToSemi :: String -> H EscapedChar
chompToSemi (';':cs) = lexer cs
chompToSemi (_:cs)   = chompToSemi cs           
chompToSemi []       = emptyH


-- | Special processing for @amp@ because it is so common.
--
-- Are there other cases to add to this function?
--
specialEscape :: String -> EscapedChar
specialEscape ['a','m','p'] = CharEscName "ampersand"
specialEscape cs            = CharEscName cs
