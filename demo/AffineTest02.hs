{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- SCALE tests
--------------------------------------------------------------------------------


module AffineTest02 where

import AffineTestBase
import Wumpus.Core

main :: IO ()
main = runAlgs [ text_ata, circle_ata, ellipse_ata, path_ata ]
               [ circle_cpa, ellipse_cpa, path_cpa ]


scale_onehalf_x_two :: Scale t => t -> t
scale_onehalf_x_two = scale 1.5 2.0


scale_onehalf_x_twoP :: Primitive -> Primitive
scale_onehalf_x_twoP = scale 1.5 2.0

-- Primitive - Text


text_ata :: AffineTrafoAlg
text_ata = AffineTrafoAlg
    { ata_console_msg       = "Scaled text..."
    , ata_eps_file          = "./out/affine_test/scale_text.eps"
    , ata_svg_file          = "./out/affine_test/scale_text.svg"
    , ata_prim_constructor  = rgbLabel
    , ata_pic_transformer   = scale_onehalf_x_two
    , ata_prim_transformer  = scale_onehalf_x_twoP
    }

circle_ata :: AffineTrafoAlg
circle_ata = AffineTrafoAlg 
    { ata_console_msg       = "Scaled circle..."
    , ata_eps_file          = "./out/affine_test/scale_circle.eps"
    , ata_svg_file          = "./out/affine_test/scale_circle.svg"
    , ata_prim_constructor  = rgbCircle
    , ata_pic_transformer   = scale_onehalf_x_two
    , ata_prim_transformer  = scale_onehalf_x_twoP
    }


ellipse_ata :: AffineTrafoAlg
ellipse_ata = AffineTrafoAlg
    { ata_console_msg       = "Scaled ellipse..."
    , ata_eps_file          = "./out/affine_test/scale_ellipse.eps"
    , ata_svg_file          = "./out/affine_test/scale_ellipse.svg"
    , ata_prim_constructor  = rgbEllipse
    , ata_pic_transformer   = scale_onehalf_x_two
    , ata_prim_transformer  = scale_onehalf_x_twoP
    }

path_ata :: AffineTrafoAlg
path_ata = AffineTrafoAlg
    { ata_console_msg       = "Scaled path..."
    , ata_eps_file          = "./out/affine_test/scale_path.eps"
    , ata_svg_file          = "./out/affine_test/scale_path.svg"
    , ata_prim_constructor  = rgbPath
    , ata_pic_transformer   = scale_onehalf_x_two
    , ata_prim_transformer  = scale_onehalf_x_twoP
    }


--------------------
--------------------

circle_cpa :: ControlPointAlg
circle_cpa = ControlPointAlg 
    { cpa_console_msg       = "Scaled circle (control points) ..."
    , cpa_eps_file          = "./out/affine_test/scale_crc_cp.eps"
    , cpa_svg_file          = "./out/affine_test/scale_crc_cp.svg"
    , cpa_prim_constructor  = rgbCircle
    , cpa_prim_transformer  = scale_onehalf_x_twoP
    }



ellipse_cpa :: ControlPointAlg
ellipse_cpa = ControlPointAlg 
    { cpa_console_msg       = "Scaled ellipse (control points) ..."
    , cpa_eps_file          = "./out/affine_test/scale_ell_cp.eps"
    , cpa_svg_file          = "./out/affine_test/scale_ell_cp.svg"
    , cpa_prim_constructor  = rgbEllipse
    , cpa_prim_transformer  = scale_onehalf_x_twoP
    }


path_cpa :: ControlPointAlg
path_cpa = ControlPointAlg 
    { cpa_console_msg       = "Path (control points)..."
    , cpa_eps_file          = "./out/affine_test/scale_path_cp.eps"
    , cpa_svg_file          = "./out/affine_test/scale_path_cp.svg"
    , cpa_prim_constructor  = rgbPath
    , cpa_prim_transformer  = scale_onehalf_x_twoP
    }

