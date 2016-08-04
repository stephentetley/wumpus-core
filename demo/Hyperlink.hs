{-# OPTIONS -Wall #-}

module Hyperlink where

import Wumpus.Core

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS "./out/hyperlink01.eps"  link_pic
    writeSVG "./out/hyperlink01.svg"  link_pic


link_pic :: Picture
link_pic = frame [ xlinkPrim xref $ ztextlabel "www.haskell.org" zeroPt ]
  where    
     xref = xlinkhref "http://www.haskell.org"


