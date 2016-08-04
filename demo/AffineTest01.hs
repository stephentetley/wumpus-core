{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- ROTATE tests
--------------------------------------------------------------------------------

module AffineTest01 where


import AffineTestBase
import Wumpus.Core


main :: IO ()
main = runAlgs [ text_ata, circle_ata, ellipse_ata, path_ata ]
               [ circle_cpa, ellipse_cpa, path_cpa ]


rot30 :: Rotate t => t -> t
rot30 = rotate30

rot30P :: Primitive -> Primitive
rot30P = rotate30

-- Primitive - Text


text_ata :: AffineTrafoAlg
text_ata = AffineTrafoAlg
    { ata_console_msg       = "Rotate text..."
    , ata_eps_file          = "./out/affine_test/rotate_text.eps"
    , ata_svg_file          = "./out/affine_test/rotate_text.svg"
    , ata_prim_constructor  = rgbLabel
    , ata_pic_transformer   = rot30
    , ata_prim_transformer  = rot30P
    }

circle_ata :: AffineTrafoAlg
circle_ata = AffineTrafoAlg 
    { ata_console_msg       = "Rotate circle..."
    , ata_eps_file          = "./out/affine_test/rotate_circle.eps"
    , ata_svg_file          = "./out/affine_test/rotate_circle.svg"
    , ata_prim_constructor  = rgbCircle
    , ata_pic_transformer   = rot30
    , ata_prim_transformer  = rot30P
    }


ellipse_ata :: AffineTrafoAlg
ellipse_ata = AffineTrafoAlg
    { ata_console_msg       = "Rotate ellipse..."
    , ata_eps_file          = "./out/affine_test/rotate_ellipse.eps"
    , ata_svg_file          = "./out/affine_test/rotate_ellipse.svg"
    , ata_prim_constructor  = rgbEllipse
    , ata_pic_transformer   = rot30
    , ata_prim_transformer  = rot30P
    }

path_ata :: AffineTrafoAlg
path_ata = AffineTrafoAlg
    { ata_console_msg       = "Rotate path..."
    , ata_eps_file          = "./out/affine_test/rotate_path.eps"
    , ata_svg_file          = "./out/affine_test/rotate_path.svg"
    , ata_prim_constructor  = rgbPath
    , ata_pic_transformer   = rot30
    , ata_prim_transformer  = rot30P
    }


--------------------
--------------------

circle_cpa :: ControlPointAlg
circle_cpa = ControlPointAlg 
    { cpa_console_msg       = "Rotate circle (control points) ..."
    , cpa_eps_file          = "./out/affine_test/rotate_crc_cp.eps"
    , cpa_svg_file          = "./out/affine_test/rotate_crc_cp.svg"
    , cpa_prim_constructor  = rgbCircle
    , cpa_prim_transformer  = rot30P
    }



ellipse_cpa :: ControlPointAlg
ellipse_cpa = ControlPointAlg 
    { cpa_console_msg       = "Rotate ellipse (control points) ..."
    , cpa_eps_file          = "./out/affine_test/rotate_ell_cp.eps"
    , cpa_svg_file          = "./out/affine_test/rotate_ell_cp.svg"
    , cpa_prim_constructor  = rgbEllipse
    , cpa_prim_transformer  = rot30P
    }


path_cpa :: ControlPointAlg
path_cpa = ControlPointAlg 
    { cpa_console_msg       = "Rotate path (control points)..."
    , cpa_eps_file          = "./out/affine_test/rotate_path_cp.eps"
    , cpa_svg_file          = "./out/affine_test/rotate_path_cp.svg"
    , cpa_prim_constructor  = rgbPath
    , cpa_prim_transformer  = rot30P
    }

