{-# OPTIONS -Wall #-}

> ------------------------------------------------------------------------------
> -- | 
> -- Module       :   Wumpus.Core.Text.Symbol
> -- Copyright    :   (c) Stephen Tetley 2010
> -- License      :   BSD3
> -- 
> -- Maintainer   :   Stephen Tetley <stephen.tetley@gmail.com>
> -- Stability    :   unstable
> -- Portability  :   GHC
> -- 
> -- Encoding vector for the Symbol font.
> -- 
> -- \*\* This file is auto-generated. \*\*
> -- 
> -- Generated - 2010-11-17 15:24
> -- 
> ------------------------------------------------------------------------------

> module Wumpus.Core.Text.Symbol
>   (
>      symbol_encoding
>   ) where

> import Wumpus.Core.Text.Base

> import qualified Data.IntMap as IntMap


> -- | Table mapping character numbers to glyph names for the
> -- Symbol font.

> symbol_encoding :: EncodingVector
> symbol_encoding = IntMap.fromAscList $
>     [ ( 0x0020, "space" )
>     , ( 0x0021, "exclam" )
>     , ( 0x0022, "universal" )
>     , ( 0x0023, "numbersign" )
>     , ( 0x0024, "existential" )
>     , ( 0x0025, "percent" )
>     , ( 0x0026, "ampersand" )
>     , ( 0x0027, "suchthat" )
>     , ( 0x0028, "parenleft" )
>     , ( 0x0029, "parenright" )
>     , ( 0x002a, "asteriskmath" )
>     , ( 0x002b, "plus" )
>     , ( 0x002c, "comma" )
>     , ( 0x002d, "minus" )
>     , ( 0x002e, "period" )
>     , ( 0x002f, "slash" )
>     , ( 0x0030, "zero" )
>     , ( 0x0031, "one" )
>     , ( 0x0032, "two" )
>     , ( 0x0033, "three" )
>     , ( 0x0034, "four" )
>     , ( 0x0035, "five" )
>     , ( 0x0036, "six" )
>     , ( 0x0037, "seven" )
>     , ( 0x0038, "eight" )
>     , ( 0x0039, "nine" )
>     , ( 0x003a, "colon" )
>     , ( 0x003b, "semicolon" )
>     , ( 0x003c, "less" )
>     , ( 0x003d, "equal" )
>     , ( 0x003e, "greater" )
>     , ( 0x003f, "question" )
>     , ( 0x0040, "congruent" )
>     , ( 0x0041, "Alpha" )
>     , ( 0x0042, "Beta" )
>     , ( 0x0043, "Chi" )
>     , ( 0x0044, "Delta" )
>     , ( 0x0045, "Epsilon" )
>     , ( 0x0046, "Phi" )
>     , ( 0x0047, "Gamma" )
>     , ( 0x0048, "Eta" )
>     , ( 0x0049, "Iota" )
>     , ( 0x004a, "theta1" )
>     , ( 0x004b, "Kappa" )
>     , ( 0x004c, "Lambda" )
>     , ( 0x004d, "Mu" )
>     , ( 0x004e, "Nu" )
>     , ( 0x004f, "Omicron" )
>     , ( 0x0050, "Pi" )
>     , ( 0x0051, "Theta" )
>     , ( 0x0052, "Rho" )
>     , ( 0x0053, "Sigma" )
>     , ( 0x0054, "Tau" )
>     , ( 0x0055, "Upsilon" )
>     , ( 0x0056, "sigma1" )
>     , ( 0x0057, "Omega" )
>     , ( 0x0058, "Xi" )
>     , ( 0x0059, "Psi" )
>     , ( 0x005a, "Zeta" )
>     , ( 0x005b, "bracketleft" )
>     , ( 0x005c, "therefore" )
>     , ( 0x005d, "bracketright" )
>     , ( 0x005e, "perpendicular" )
>     , ( 0x005f, "underscore" )
>     , ( 0x0060, "radicalex" )
>     , ( 0x0061, "alpha" )
>     , ( 0x0062, "beta" )
>     , ( 0x0063, "chi" )
>     , ( 0x0064, "delta" )
>     , ( 0x0065, "epsilon" )
>     , ( 0x0066, "phi" )
>     , ( 0x0067, "gamma" )
>     , ( 0x0068, "eta" )
>     , ( 0x0069, "iota" )
>     , ( 0x006a, "phi1" )
>     , ( 0x006b, "kappa" )
>     , ( 0x006c, "lambda" )
>     , ( 0x006d, "mu" )
>     , ( 0x006e, "nu" )
>     , ( 0x006f, "omicron" )
>     , ( 0x0070, "pi" )
>     , ( 0x0071, "theta" )
>     , ( 0x0072, "rho" )
>     , ( 0x0073, "sigma" )
>     , ( 0x0074, "tau" )
>     , ( 0x0075, "upsilon" )
>     , ( 0x0076, "omega1" )
>     , ( 0x0077, "omega" )
>     , ( 0x0078, "xi" )
>     , ( 0x0079, "psi" )
>     , ( 0x007a, "zeta" )
>     , ( 0x007b, "braceleft" )
>     , ( 0x007c, "bar" )
>     , ( 0x007d, "braceright" )
>     , ( 0x007e, "similar" )
>     , ( 0x00a0, "Euro" )
>     , ( 0x00a1, "Upsilon1" )
>     , ( 0x00a2, "minute" )
>     , ( 0x00a3, "lessequal" )
>     , ( 0x00a4, "fraction" )
>     , ( 0x00a5, "infinity" )
>     , ( 0x00a6, "florin" )
>     , ( 0x00a7, "club" )
>     , ( 0x00a8, "diamond" )
>     , ( 0x00a9, "heart" )
>     , ( 0x00aa, "spade" )
>     , ( 0x00ab, "arrowboth" )
>     , ( 0x00ac, "arrowleft" )
>     , ( 0x00ad, "arrowup" )
>     , ( 0x00ae, "arrowright" )
>     , ( 0x00af, "arrowdown" )
>     , ( 0x00b0, "degree" )
>     , ( 0x00b1, "plusminus" )
>     , ( 0x00b2, "second" )
>     , ( 0x00b3, "greaterequal" )
>     , ( 0x00b4, "multiply" )
>     , ( 0x00b5, "proportional" )
>     , ( 0x00b6, "partialdiff" )
>     , ( 0x00b7, "bullet" )
>     , ( 0x00b8, "divide" )
>     , ( 0x00b9, "notequal" )
>     , ( 0x00ba, "equivalence" )
>     , ( 0x00bb, "approxequal" )
>     , ( 0x00bc, "ellipsis" )
>     , ( 0x00bd, "arrowvertex" )
>     , ( 0x00be, "arrowhorizex" )
>     , ( 0x00bf, "carriagereturn" )
>     , ( 0x00c0, "aleph" )
>     , ( 0x00c1, "Ifraktur" )
>     , ( 0x00c2, "Rfraktur" )
>     , ( 0x00c3, "weierstrass" )
>     , ( 0x00c4, "circlemultiply" )
>     , ( 0x00c5, "circleplus" )
>     , ( 0x00c6, "emptyset" )
>     , ( 0x00c7, "intersection" )
>     , ( 0x00c8, "union" )
>     , ( 0x00c9, "propersuperset" )
>     , ( 0x00ca, "reflexsuperset" )
>     , ( 0x00cb, "notsubset" )
>     , ( 0x00cc, "propersubset" )
>     , ( 0x00cd, "reflexsubset" )
>     , ( 0x00ce, "element" )
>     , ( 0x00cf, "notelement" )
>     , ( 0x00d0, "angle" )
>     , ( 0x00d1, "gradient" )
>     , ( 0x00d2, "registerserif" )
>     , ( 0x00d3, "copyrightserif" )
>     , ( 0x00d4, "trademarkserif" )
>     , ( 0x00d5, "product" )
>     , ( 0x00d6, "radical" )
>     , ( 0x00d7, "dotmath" )
>     , ( 0x00d8, "logicalnot" )
>     , ( 0x00d9, "logicaland" )
>     , ( 0x00da, "logicalor" )
>     , ( 0x00db, "arrowdblboth" )
>     , ( 0x00dc, "arrowdblleft" )
>     , ( 0x00dd, "arrowdblup" )
>     , ( 0x00de, "arrowdblright" )
>     , ( 0x00df, "arrowdbldown" )
>     , ( 0x00e0, "lozenge" )
>     , ( 0x00e1, "angleleft" )
>     , ( 0x00e2, "registersans" )
>     , ( 0x00e3, "copyrightsans" )
>     , ( 0x00e4, "trademarksans" )
>     , ( 0x00e5, "summation" )
>     , ( 0x00e6, "parenlefttp" )
>     , ( 0x00e7, "parenleftex" )
>     , ( 0x00e8, "parenleftbt" )
>     , ( 0x00e9, "bracketlefttp" )
>     , ( 0x00ea, "bracketleftex" )
>     , ( 0x00eb, "bracketleftbt" )
>     , ( 0x00ec, "bracelefttp" )
>     , ( 0x00ed, "braceleftmid" )
>     , ( 0x00ee, "braceleftbt" )
>     , ( 0x00ef, "braceex" )
>     , ( 0x00f1, "angleright" )
>     , ( 0x00f2, "integral" )
>     , ( 0x00f3, "integraltp" )
>     , ( 0x00f4, "integralex" )
>     , ( 0x00f5, "integralbt" )
>     , ( 0x00f6, "parenrighttp" )
>     , ( 0x00f7, "parenrightex" )
>     , ( 0x00f8, "parenrightbt" )
>     , ( 0x00f9, "bracketrighttp" )
>     , ( 0x00fa, "bracketrightex" )
>     , ( 0x00fb, "bracketrightbt" )
>     , ( 0x00fc, "bracerighttp" )
>     , ( 0x00fd, "bracerightmid" )
>     , ( 0x00fe, "bracerightbt" )
>     ]