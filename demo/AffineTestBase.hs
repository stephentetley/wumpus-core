{-# OPTIONS -Wall #-}

-- Common machinery for the affine tests.

module AffineTestBase 
  ( 
    -- * test common code
    runAlgs
  , AffineTrafoAlg(..)
  , ControlPointAlg(..)

  , rgbLabel
  , rgbCircle
  , rgbEllipse
  , rgbPath

  ) where


import Wumpus.Core
import Wumpus.Core.Colour ( black, red, blue )



import System.Directory


light_blue :: RGBi
light_blue = RGBi 176 224 231


runAlgs :: [AffineTrafoAlg] -> [ControlPointAlg] -> IO ()
runAlgs ats cps = mkDirs >> mapM_ runATA ats >> mapM_ runCPA cps
  where
    mkDirs = createDirectoryIfMissing True "./out/affine_test/"


data AffineTrafoAlg = AffineTrafoAlg
      { ata_console_msg         :: String
      , ata_eps_file            :: FilePath
      , ata_svg_file            :: FilePath
      , ata_prim_constructor    :: RGBi -> Primitive
      , ata_pic_transformer     :: Picture -> Picture
      , ata_prim_transformer    :: Primitive -> Primitive
      }

runATA :: AffineTrafoAlg -> IO ()
runATA ata = do 
    { putStrLn $ ata_console_msg ata
    ; writeEPS (ata_eps_file ata) pic
    ; writeSVG (ata_svg_file ata) pic 
    }
  where
    pic = buildPictureATA (ata_prim_constructor ata) 
                          (ata_pic_transformer  ata)
                          (ata_prim_transformer ata)


buildPictureATA :: (RGBi -> Primitive) 
         -> (Picture -> Picture) 
         -> (Primitive -> Primitive) 
         -> Picture
buildPictureATA mk picF primF = 
    picture1 `picBeside` picture2 `picBeside` picture3
  where
    picture1 :: Picture
    picture1 = illustrateBounds light_blue $ frame [mk black]
  
    picture2 :: Picture
    picture2 = illustrateBounds light_blue $ picF $ frame [mk blue]

    picture3 :: Picture
    picture3 = illustrateBoundsPrim light_blue prim
      where
        prim :: Primitive
        prim = primF $ mk red





--------------------------------------------------------------------------------

data ControlPointAlg = ControlPointAlg
      { cpa_console_msg         :: String
      , cpa_eps_file            :: FilePath
      , cpa_svg_file            :: FilePath
      , cpa_prim_constructor    :: RGBi -> Primitive
      , cpa_prim_transformer    :: Primitive -> Primitive
      }

runCPA :: ControlPointAlg -> IO ()
runCPA cpa = do 
    { putStrLn $ cpa_console_msg cpa
    ; writeEPS (cpa_eps_file cpa) pic
    ; writeSVG (cpa_svg_file cpa) pic
    }
  where
    pic = cpPicture (cpa_prim_constructor cpa) (cpa_prim_transformer cpa)

cpPicture :: (RGBi -> Primitive) -> (Primitive -> Primitive) -> Picture
cpPicture constr trafo = 
    illustrateBounds light_blue $ illustrateControlPoints black 
                                $ transformed_prim
  where
   transformed_prim :: Primitive
   transformed_prim = trafo $ constr red


--------------------------------------------------------------------------------

rgbLabel :: RGBi -> Primitive
rgbLabel rgb = textlabel rgb wumpus_default_font "Wumpus!" zeroPt

rgbCircle :: RGBi -> Primitive
rgbCircle rgb = fillEllipse rgb 60 60 zeroPt

rgbEllipse :: RGBi -> Primitive
rgbEllipse rgb = fillEllipse rgb 60 30 zeroPt

rgbPath :: RGBi -> Primitive
rgbPath rgb = ostroke rgb default_stroke_attr $ dog_kennel

--------------------------------------------------------------------------------
-- Demo - draw a dog kennel...


dog_kennel :: PrimPath
dog_kennel = absPrimPath zeroPt $ 
    [ absLineTo  (P2 0 60) 
    , absLineTo  (P2 40 100)
    , absLineTo  (P2 80 60)
    , absLineTo  (P2 80 0)
    , absLineTo  (P2 60 0)  
    , absLineTo  (P2 60 30)
    , absCurveTo (P2 60 50) (P2 50 60) (P2 40 60)
    , absCurveTo (P2 30 60) (P2 20 50) (P2 20 30)
    , absLineTo  (P2 20 0)
    ]