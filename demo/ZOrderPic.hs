{-# OPTIONS -Wall #-}

module ZOrderPic where

import Wumpus.Core
import Wumpus.Core.Colour

import Data.AffineSpace                 -- package: vector-space

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    putStrLn zorder_msg
    writeEPS "./out/zorder01.eps" combined_pic
    writeSVG "./out/zorder01.svg" combined_pic

zorder_msg :: String
zorder_msg = unlines $ 
    [ ""
    , "Wumpus-core draws lists of Primitives left-to-right."
    , "The head of the list will be drawn first - it will appear"
    , "at the top of the output file. It will also be drawn at"
    , "the back of the Z-Order."
    , ""
    , "In the generated file, you should see:"
    , "  red `under` green, green `under` blue"
    , ""
    ]

combined_pic :: Picture
combined_pic = multi [pic1,pic2]

pic1 :: Picture
pic1 = frame $ prim_list zeroPt

pic2 :: Picture 
pic2 = multi $ map (\a -> frame [a]) $ prim_list (P2 200 0)

prim_list :: DPoint2 -> [Primitive]
prim_list = sequence [ fillEllipse red   20 20
                     , \p -> fillEllipse green 20 20 (p .+^ hvec 20)
                     , \p -> fillEllipse blue  20 20 (p .+^ hvec 40)
                     ]

