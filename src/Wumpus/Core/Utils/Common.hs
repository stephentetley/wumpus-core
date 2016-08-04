{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Utils.Common
-- Copyright   :  (c) Stephen Tetley 2009-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Utility functions and a Hughes list.
--
--------------------------------------------------------------------------------


module Wumpus.Core.Utils.Common
  ( 

  -- * Truncate / print a double
    dtruncFmt

  , truncateDouble
  , roundup
  


  -- * PostScript time stamp
  , psTimeStamp


  ) where


import qualified Wumpus.Core.Utils.FormatCombinators as Fmt


import Data.Time




--------------------------------------------------------------------------------
-- PS Unit

-- 
dtruncFmt :: Double -> Fmt.Doc
dtruncFmt = Fmt.text . truncateDouble


-- | Truncate the printed decimal representation of a Double.
-- The is prefered to 'showFFloat' from Numeric as it produces
-- shorter representations where appropriate.
-- 
-- 0.000000000 becomes 0.0 rather than however many digs are 
-- specified.
--  
truncateDouble :: Double -> String
truncateDouble d | abs d < 0.0001  = "0.0"
                 | d < 0.0         = '-' :  show (abs tx)
                 | otherwise       = show tx
  where
    tx :: Double
    tx = (realToFrac (roundi (d*1000000.0))) / 1000000.0

roundi :: RealFrac a => a -> Integer
roundi = round

-- | Take 'ceilingi' and show.
roundup :: Double -> String
roundup = show . ceilingi

-- Avoid those annoying 'Defaulting ...' warnings...
ceilingi :: RealFrac a => a -> Integer
ceilingi = ceiling




--------------------------------------------------------------------------------


-- | To be used with getZonedTime

psTimeStamp :: ZonedTime -> ShowS
psTimeStamp zt = localTimeS . showChar ' ' . localDayS
  where
    local_tim   = zonedTimeToLocalTime zt
    localTimeS  = timeOfDay  $ localTimeOfDay $ local_tim
    localDayS   = showString $ showGregorian  $ localDay local_tim

timeOfDay :: TimeOfDay -> ShowS
timeOfDay t = 
    fn todHour . showChar ':' . fn todMin . showChar ':' . fn (floori . todSec)
  where
    fn f = pad2 (f t) 


pad2 :: Int -> ShowS
pad2 i | i < 10    = ('0':) . shows i
       | otherwise = shows i  


floori :: RealFrac a => a -> Int
floori = floor


