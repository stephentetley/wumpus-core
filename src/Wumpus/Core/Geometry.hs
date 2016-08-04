{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Geometry
-- Copyright   :  (c) Stephen Tetley 2009-2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Objects and operations for 2D geometry.
-- 
-- Vector, point, 3x3 matrix, and radian representations, 
-- plus a type family @DUnit@ for parameterizing type classes 
-- with some /dimension/.
--
--------------------------------------------------------------------------------


module Wumpus.Core.Geometry
  ( 

    -- * Type family 
    DUnit

  , Tolerance(..)


  -- * Data types
  , Vec2(..)
  , DVec2
  , Point2(..)
  , DPoint2
  , Matrix3'3(..)
  , DMatrix3'3

  , Radian

  , MatrixMult(..)


  -- * Tolerance helpers
  , tEQ
  , tGT
  , tLT
  , tGTE
  , tLTE
  , tCompare

  -- * Vector operations
  , zeroVec
  , vec
  , hvec
  , vvec
  , avec
  , pvec
  , orthoVec
  , vreverse
  , vdirection
  , vlength
  , vangle
  , vsum
  , vdiff

  -- * Point operations
  , zeroPt
  , minPt
  , maxPt
  , lineDirection

  -- * Matrix contruction
  , identityMatrix
  , scalingMatrix
  , translationMatrix
  , rotationMatrix
  , originatedRotationMatrix

  -- * Matrix operations
  , invert
  , determinant
  , transpose

  -- * Radian operations
  , toRadian
  , fromRadian
  , d2r
  , r2d
  , circularModulo

  -- * Bezier curves
  , bezierCircle
  , bezierEllipse
  , rbezierEllipse

  , bezierArc
  , subdivisionCircle

  ) where


import Wumpus.Core.Utils.FormatCombinators

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace



--------------------------------------------------------------------------------

-- | Some unit of dimension usually Double.
--
-- This very useful for reducing the kind of type classes to *.
-- 
-- Then constraints on the Unit type can be declared on the 
-- instances rather than in the class declaration.
-- 
type family DUnit a :: *


-- Not exported - thanks to Max Bollingbroke.
--
type family   GuardEq a b :: *
type instance GuardEq a a = a




-- | Class for tolerance on floating point numbers.
-- 
-- Two tolerances are required tolerance for equality - commonly 
-- used for testing if two points are equal - and tolerance for 
-- path length measurement.
-- 
-- Path length measurement in Wumpus does not have a strong 
-- need to be exact (precision is computational costly) - by 
-- default it is 100x the equality tolerance.
-- 
-- Bezier path lengths are calculated by iteration, so greater 
-- accuracy requires more compution. As it is hard to visually
-- differentiate measures of less than a point the tolerance 
-- for Points is quite high quite high (0.1).
-- 
-- The situation is more complicated for contextual units 
-- (Em and En) as they are really scaling factors. The bigger
-- the point size the less accurate the measure is.
-- 
class Num u => Tolerance u where 
  eq_tolerance     :: u
  length_tolerance :: u

  length_tolerance = 100 * eq_tolerance 



instance Tolerance Double where 
  eq_tolerance     = 0.001
  length_tolerance = 0.1



-- Datatypes 


-- | 2D Vector - both components are strict.
--
-- Note - equality is defined with 'Tolerance' and tolerance is 
-- quite high for the usual units. See the note for 'Point2'.
-- 
data Vec2 u = V2 
      { vector_x :: !u 
      , vector_y :: !u
      }
  deriving (Show)

type DVec2 = Vec2 Double



-- | 2D Point - both components are strict.
-- 
-- Note - equality is defined with 'Tolerance' and tolerance is 
-- quite high for the usual units. 
-- 
-- This is useful for drawing, *but* unacceptable data centric 
-- work. If more accurate equality is needed define a newtype
-- wrapper over the unit type and make a @Tolerance@ instance with 
-- much greater accuracy.
--
data Point2 u = P2 
      { point_x    :: !u
      , point_y    :: !u
      }
  deriving (Show)

type DPoint2 = Point2 Double



-- | 3x3 matrix, considered to be in row-major form.
-- 
-- > (M3'3 a b c
-- >       d e f
-- >       g h i)
--
-- For instance the rotation matrix is represented as
--
-- >  ( cos(a) -sin(a) 0
-- >    sin(a)  cos(a) 0  
-- >      0         0  1 )
--
-- This seems commplace in geometry texts, but PostScript 
-- represents the @current-transformation-matrix@  in 
-- column-major form.
--
-- The right-most column is considered to represent a
-- coordinate:
--
-- >  ( 1 0 x
-- >    0 1 y  
-- >    0 0 1 ) 
-- >
-- 
-- So a translation matrix representing the displacement in x 
-- of 40 and in y of 10 would be:
--
-- >  ( 1 0 40
-- >    0 1 10  
-- >    0 0 1  ) 
-- >
-- 


data Matrix3'3 u = M3'3 !u !u !u  !u !u !u  !u !u !u
  deriving (Eq)

type DMatrix3'3 = Matrix3'3 Double


-- | Radian is represented with a distinct type. 
-- Equality and ordering are approximate where the epsilon 
-- is 0.0001.
newtype Radian = Radian { getRadian :: Double }
  deriving (Num,Real,Fractional,Floating,RealFrac,RealFloat)


--------------------------------------------------------------------------------
-- Family instances

type instance DUnit (Point2 u)      = u
type instance DUnit (Vec2 u)        = u
type instance DUnit (Matrix3'3 u)   = u

type instance DUnit (Maybe a)       = DUnit a
type instance DUnit (a,b)           = GuardEq (DUnit a) (DUnit b)

--------------------------------------------------------------------------------
-- lifters / convertors

lift2Vec2 :: (u -> u -> u) -> Vec2 u -> Vec2 u -> Vec2 u
lift2Vec2 op (V2 x y) (V2 x' y') = V2 (x `op` x') (y `op` y')


lift2Matrix3'3 :: (u -> u -> u) -> Matrix3'3 u -> Matrix3'3 u -> Matrix3'3 u
lift2Matrix3'3 op (M3'3 a b c d e f g h i) (M3'3 m n o p q r s t u) = 
      M3'3 (a `op` m) (b `op` n) (c `op` o)  
           (d `op` p) (e `op` q) (f `op` r)  
           (g `op` s) (h `op` t) (i `op` u)


--------------------------------------------------------------------------------
-- instances

-- Eq (with tolerance)

instance (Tolerance u, Ord u) => Eq (Vec2 u) where
  V2 x0 y0 == V2 x1 y1 = x0 `tEQ` x1 && y0 `tEQ` y1


instance (Tolerance u, Ord u) => Eq (Point2 u) where
  P2 x0 y0 == P2 x1 y1 = x0 `tEQ` x1 && y0 `tEQ` y1


-- Ord (with Tolerance)

instance (Tolerance u, Ord u) => Ord (Vec2 u) where
  V2 x0 y0 `compare` V2 x1 y1 = case tCompare x0 x1 of
                                  EQ  -> tCompare y0 y1
                                  ans -> ans


instance (Tolerance u, Ord u) => Ord (Point2 u) where
  P2 x0 y0 `compare` P2 x1 y1 = case tCompare x0 x1 of
                                  EQ  -> tCompare y0 y1
                                  ans -> ans




-- Functor

instance Functor Vec2 where
  fmap f (V2 a b) = V2 (f a) (f b)


instance Functor Point2 where
  fmap f (P2 a b) = P2 (f a) (f b)


instance Functor Matrix3'3 where
  fmap f (M3'3 m n o   p q r   s t u) = 
    M3'3 (f m) (f n) (f o)   (f p) (f q) (f r)   (f s) (f t) (f u)


-- Show

instance Show u => Show (Matrix3'3 u) where
  show (M3'3 a b c d e f g h i) = "(M3'3 " ++ body ++ ")" where
    body = show [[a,b,c],[d,e,f],[g,h,i]]

-- Num



instance Num u => Num (Matrix3'3 u) where
  (+) = lift2Matrix3'3 (+) 
  (-) = lift2Matrix3'3 (-)

  (*) (M3'3 a b c d e f g h i) (M3'3 m n o p q r s t u) = 
      M3'3 (a*m+b*p+c*s) (a*n+b*q+c*t) (a*o+b*r+c*u) 
           (d*m+e*p+f*s) (d*n+e*q+f*t) (d*o+e*r+f*u) 
           (g*m+h*p+i*s) (g*n+h*q+i*t) (g*o+h*r+i*u) 
  
  abs    = fmap abs 
  negate = fmap negate
  signum = fmap signum
  fromInteger a = M3'3 a' a' a'  a' a' a'  a' a' a' where a' = fromInteger a 

--------------------------------------------------------------------------------
-- Instances for Radian which are 'special'.

instance Show Radian where
  showsPrec i (Radian a) = showsPrec i a

instance Eq Radian where (==) = req

instance Ord Radian where
  compare a b | a `req` b = EQ
              | otherwise = getRadian a `compare` getRadian b



--------------------------------------------------------------------------------
-- Pretty printing

instance Format u => Format (Vec2 u) where
  format (V2 a b) = parens (text "Vec" <+> format a <+> format b)

instance Format u => Format (Point2 u) where
  format (P2 a b) = parens (format a >< comma <+> format b)

instance Format u => Format (Matrix3'3 u) where
  format (M3'3 a b c  d e f  g h i) = 
      vcat [matline a b c, matline d e f, matline g h i]
    where
      matline x y z = char '|' 
         <+> (hcat $ map (fill 12 . format) [x,y,z]) 
         <+> char '|'   


instance Format Radian where
  format (Radian d) = double d >< text ":rad"

--------------------------------------------------------------------------------
-- Vector space instances

instance Num u => AdditiveGroup (Vec2 u) where
  zeroV = V2 0 0 
  (^+^) = lift2Vec2 (+)  
  negateV = fmap negate 


instance Num u => VectorSpace (Vec2 u) where
  type Scalar (Vec2 u) = u
  s *^ v = fmap (s*) v


-- scalar (dot / inner) product via the class InnerSpace
--
-- This definition mandates UndecidableInstances, but this seems
-- in line with Data.VectorSpace...
--

instance (Num u, InnerSpace u, u ~ Scalar u) 
    => InnerSpace (Vec2 u) where
  (V2 a b) <.> (V2 a' b') = (a <.> a') ^+^ (b <.> b')


instance Num u => AffineSpace (Point2 u) where
  type Diff (Point2 u) = Vec2 u
  (P2 a b) .-. (P2 x y)   = V2 (a-x)  (b-y)
  (P2 a b) .+^ (V2 vx vy) = P2 (a+vx) (b+vy)


instance Num u => AdditiveGroup (Matrix3'3 u) where
  zeroV = fromInteger 0
  (^+^) = (+)
  negateV = negate


instance Num u => VectorSpace (Matrix3'3 u) where
  type Scalar (Matrix3'3 u) = u
  s *^ m = fmap (s*) m 


--------------------------------------------------------------------------------
-- Matrix multiply


infixr 7 *# 

-- | Matrix multiplication - typically of points and vectors 
-- represented as homogeneous coordinates. 
--
class MatrixMult t where 
  (*#) :: Num u => Matrix3'3 u -> t u -> t u


instance MatrixMult Vec2 where       
  (M3'3 a b c d e f _ _ _) *# (V2 m n) = V2 (a*m + b*n + c*0) 
                                            (d*m + e*n + f*0)


instance MatrixMult Point2 where
  (M3'3 a b c d e f _ _ _) *# (P2 m n) = P2 (a*m + b*n + c*1) 
                                            (d*m + e*n + f*1)


--------------------------------------------------------------------------------

infix 4 `tEQ`, `tLT`, `tGT`

-- | Tolerant equality - helper function for defining Eq instances
-- that use tolerance.
--
-- Note - the definition actually needs Ord which is 
-- unfortunate (as Ord is /inaccurate/).
--
tEQ :: (Tolerance u, Ord u) => u -> u -> Bool
tEQ a b = (abs (a-b)) < eq_tolerance

-- | Tolerant less than.
--
-- Note - the definition actually needs Ord which is 
-- unfortunate (as Ord is /inaccurate/).
--
tLT :: (Tolerance u, Ord u) => u -> u -> Bool
tLT a b = a < b && (b - a) > eq_tolerance


-- | Tolerant greater than.
--
-- Note - the definition actually needs Ord which is 
-- unfortunate (as Ord is /inaccurate/).
--
tGT :: (Tolerance u, Ord u) => u -> u -> Bool
tGT a b = a > b && (a - b) > eq_tolerance



-- | Tolerant less than or equal.
--
-- Note - the definition actually needs Ord which is 
-- unfortunate (as Ord is /inaccurate/).
--
tLTE :: (Tolerance u, Ord u) => u -> u -> Bool
tLTE a b = tEQ a b || tLT a b


-- | Tolerant greater than or equal.
--
-- Note - the definition actually needs Ord which is 
-- unfortunate (as Ord is /inaccurate/).
--
tGTE :: (Tolerance u, Ord u) => u -> u -> Bool
tGTE a b = tEQ a b || tGT a b


-- | Tolerant @compare@.
--
tCompare :: (Tolerance u, Ord u) => u -> u -> Ordering
tCompare a b | a `tEQ` b = EQ
             | otherwise = compare a b


--------------------------------------------------------------------------------
-- Vectors

-- | Construct a the empty vector (0,0).
--
-- Note - this is equivalent to @zeroV@ in @Data.AdditiveGroup@.
-- It is provided here for convenience as it may save an extra
-- module import in client code. 
--
zeroVec :: Num u => Vec2 u
zeroVec = V2 0 0



-- | 'vec' : @ x_component * y_component -> Vec2 @
--
-- A synonym for the constructor 'V2' with a Num constraint on 
-- the arguments.
--
-- Essentially this function is superfluous, but it is slightly 
-- more pleasant typographically when used in lists of vectors:
--
-- > [ vec 2 2, vvec 4, hvec 4, vec 2 2 ]
--
-- Versus:
--
-- > [ V2 2 2, vvec 4, hvec 4, V2 2 2 ]
--  
vec :: u -> u -> Vec2 u
vec = V2


-- | 'hvec' : @ x_component -> Vec2 @
--
-- Construct a vector with horizontal displacement.
--
hvec :: Num u => u -> Vec2 u
hvec d = V2 d 0

-- | 'vvec' @ y_component -> Vec2 @ 
--
-- Construct a vector with vertical displacement.
--
vvec :: Num u => u -> Vec2 u
vvec d = V2 0 d


-- | 'avec' : @ angle * distance -> Vec2 @
--
-- Construct a vector from an angle and magnitude.
--
avec :: Floating u => Radian -> u -> Vec2 u
avec theta d = V2 x y 
  where
    ang = fromRadian $ circularModulo theta
    x   = d * cos ang
    y   = d * sin ang


-- | 'pvec' : @ point_from * point_to -> Vec2 @
--
-- The vector between two points
--
-- > pvec = flip (.-.)
--
pvec :: Num u => Point2 u -> Point2 u -> Vec2 u
pvec = flip (.-.)



-- | Build a vector form its parallel and perpendicular components
-- and inclination.
--
orthoVec :: Floating u => u -> u -> Radian -> Vec2 u 
orthoVec pall perp ang = avec ang pall ^+^ avec (ang + half_pi) perp
  where
    half_pi = 0.5 * pi

-- | 'vreverse' : @ vec -> Vec2 @
--
-- Reverse a vector.
--
vreverse :: Num u => Vec2 u -> Vec2 u
vreverse (V2 x y) = V2 (-x) (-y)


-- | 'vdirection' : @ vec -> Radian @
-- 
-- Direction of a vector - i.e. the counter-clockwise angle 
-- from the x-axis.
--
vdirection :: (Floating u, Real u) => Vec2 u -> Radian
vdirection (V2 x y) = lineDirection (P2 0 0) (P2 x y)


-- | 'vlength' : @ vec -> Length @
--
-- Length of a vector.
--
vlength :: Floating u => Vec2 u -> u
vlength (V2 x y) = sqrt $ x*x + y*y


-- | 'vangle' : @ vec1 * vec2 -> Radian @
--
-- Extract the angle between two vectors.
--
vangle :: (Floating u, Real u, InnerSpace (Vec2 u)) 
       => Vec2 u -> Vec2 u -> Radian
vangle u v = realToFrac $ acos $ (u <.> v) / (magnitude u * magnitude v)


-- | Sum a list of Vectors.
--
vsum :: Num u => [Vec2 u] -> Vec2 u
vsum [] = V2 0 0
vsum (v:vs) = go v vs
  where
    go a []     = a
    go a (b:bs) = go (a ^+^ b) bs


-- | Find the /difference/ between two vectors - i.e. flipped
-- vector subtraction:
--
-- > vdiff = flip (^-^)
-- 
vdiff :: Num u => Vec2 u -> Vec2 u -> Vec2 u
vdiff  = flip (^-^)



--------------------------------------------------------------------------------
-- Points

-- | Construct a point at (0,0).
--
zeroPt :: Num u => Point2 u
zeroPt = P2 0 0


-- | 'minPt' : @ point1 * point2 -> Point2 @
--
-- Synthetic, /component-wise/ min on points. Standard 'min' and 
-- 'max' via Ord are defined lexographically on pairs, e.g.:
-- 
-- > min (1,2) (2,1) = (1,2)
-- 
-- For Points we want the component-wise min and max, that 
-- potentially synthesizes a new point, e.g:
--
-- > minPt (P2 1 2) (Pt 2 1) = Pt 1 1 
-- > maxPt (P2 1 2) (Pt 2 1) = Pt 2 2
-- 
minPt :: Ord u => Point2 u -> Point2 u -> Point2 u
minPt (P2 x y) (P2 x' y') = P2 (min x x') (min y y')


-- | 'maxPt' : @ point1 * point2 -> Point @
--
-- Synthetic, /component-wise/ max on points.  
--
-- > maxPt (P2 1 2) (Pt 2 1) = Pt 2 2
-- 
maxPt :: Ord u => Point2 u -> Point2 u -> Point2 u
maxPt (P2 x y) (P2 x' y') = P2 (max x x') (max y y')


-- | 'lineDirection' : @ start_point * end_point -> Radian @
--
-- Calculate the counter-clockwise angle between two points 
-- and the x-axis.
--
lineDirection :: (Floating u, Real u) => Point2 u -> Point2 u -> Radian
lineDirection (P2 x1 y1) (P2 x2 y2) = step (x2 - x1) (y2 - y1)
  where
    -- Special cases for continuity - the equality test should 
    -- catch both 0.0 and (-0.0).
    -- Note - there is undoubtedly a better way of doing this.

    step x y | x == 0 && y == 0 = 0 

    step x y | x == 0           = if y >=0 then 0.5*pi else 1.5*pi

    step x y | y == 0           = if x >=0 then 0 else pi

    -- north-east quadrant 
    step x y | pve x && pve y = toRadian $ atan (y/x)          
    
    -- north-west quadrant
    step x y | pve y          = pi     - (toRadian $ atan (y / abs x))

    -- south-east quadrant
    step x y | pve x          = (2*pi) - (toRadian $ atan (abs y / x)) 

    -- otherwise... south-west quadrant
    step x y                  = pi     + (toRadian $ atan (y/x))

    pve a = signum a >= 0


--------------------------------------------------------------------------------
-- Matrix construction

-- | Construct the identity matrix:
--
-- > (M3'3 1 0 0
-- >       0 1 0
-- >       0 0 1 )
--
identityMatrix :: Num u => Matrix3'3 u
identityMatrix = M3'3 1 0 0  
                      0 1 0  
                      0 0 1



-- Common transformation matrices (for 2d homogeneous coordinates)

-- | 'scalingMatrix' : @ x_scale_factor * y_scale_factor -> Matrix @
--
-- Construct a scaling matrix:
--
-- > (M3'3 sx 0  0
-- >       0  sy 0
-- >       0  0  1 )
--
scalingMatrix :: Num u => u -> u -> Matrix3'3 u
scalingMatrix sx sy = M3'3  sx 0  0   
                            0  sy 0   
                            0  0  1



-- | 'translationMatrix' : @ x_displacement * y_displacement -> Matrix @
--
-- Construct a translation matrix:
--
-- > (M3'3 1  0  x
-- >       0  1  y
-- >       0  0  1 )
--
translationMatrix :: Num u => u -> u -> Matrix3'3 u
translationMatrix x y = M3'3 1 0 x  
                             0 1 y  
                             0 0 1



-- | 'rotationMatrix' : @ ang -> Matrix @
--
-- Construct a rotation matrix:
--
-- > (M3'3 cos(a)  -sin(a)  0
-- >       sin(a)   cos(a)  0
-- >       0        0       1 )
--
rotationMatrix :: Floating u => Radian -> Matrix3'3 u
rotationMatrix a = M3'3 (cos ang) (negate $ sin ang) 0 
                        (sin ang) (cos ang)          0  
                        0         0                  1
  where ang = fromRadian a



-- No reflectionMatrix function
-- A reflection about the x-axis is a scale of 1 (-1)
-- A reflection about the y-axis is a scale of (-1) 1


-- | 'originatedRotationMatrix' : @ ang * point -> Matrix @
-- 
-- Construct a matrix for rotation about some /point/.
--
-- This is the product of three matrices: T R T^-1
-- 
-- (T being the translation matrix, R the rotation matrix and
-- T^-1 the inverse of the translation matrix).
--
originatedRotationMatrix :: Floating u
                         => Radian -> (Point2 u) -> Matrix3'3 u
originatedRotationMatrix ang (P2 x y) = mT * (rotationMatrix ang) * mTinv
  where
    mT    = M3'3 1 0 x     
                 0 1 y     
                 0 0 1

    mTinv = M3'3 1 0 (-x)  
                 0 1 (-y)  
                 0 0 1
  



--------------------------------------------------------------------------------
-- Matrix ops


-- | Invert a matrix.
--
invert :: Fractional u => Matrix3'3 u -> Matrix3'3 u
invert m = (1 / determinant m) *^ adjoint m

-- | Determinant of a matrix.
--
determinant :: Num u => Matrix3'3 u -> u
determinant (M3'3 a b c d e f g h i) = a*e*i - a*f*h - b*d*i + b*f*g + c*d*h - c*e*g

-- | Transpose a matrix.
--
transpose :: Matrix3'3 u -> Matrix3'3 u
transpose (M3'3 a b c 
                d e f 
                g h i) = M3'3 a d g  
                              b e h  
                              c f i

-- Helpers

adjoint :: Num u => Matrix3'3 u -> Matrix3'3 u
adjoint = transpose . cofactor . mofm


cofactor :: Num u => Matrix3'3 u -> Matrix3'3 u
cofactor (M3'3 a b c  
               d e f  
               g h i) = M3'3   a  (-b)   c
                             (-d)   e  (-f)
                               g  (-h)   i

mofm :: Num u => Matrix3'3 u -> Matrix3'3 u
mofm (M3'3 a b c  
               d e f  
               g h i)  = M3'3 m11 m12 m13  
                              m21 m22 m23 
                              m31 m32 m33
  where  
    m11 = (e*i) - (f*h)
    m12 = (d*i) - (f*g)
    m13 = (d*h) - (e*g)
    m21 = (b*i) - (c*h)
    m22 = (a*i) - (c*g)
    m23 = (a*h) - (b*g)
    m31 = (b*f) - (c*e)
    m32 = (a*f) - (c*d)
    m33 = (a*e) - (b*d)





--------------------------------------------------------------------------------
-- Radians


-- | The epislion used for floating point equality on radians.
--
radian_epsilon :: Double
radian_epsilon = 0.0001

-- | Equality on radians, this is the operation used for (==) in
-- Radian\'s Eq instance.
--
req :: Radian -> Radian -> Bool
req a b = (fromRadian $ abs (a-b)) < radian_epsilon



-- | Convert to radians.
--
toRadian :: Real a => a -> Radian 
toRadian = Radian . realToFrac


-- | Convert from radians.
--
fromRadian :: Fractional a => Radian -> a
fromRadian = realToFrac . getRadian


-- | Degrees to radians.
--
-- Degree type fixed to @Double@, compose @d2r@ with @realToFrac@ 
-- for @Float@ etc.
--
d2r :: Double -> Radian
d2r = Radian . realToFrac . (*) (pi/180)

-- | Radians to degrees.
--
-- Degree type fixed to @Double@, compose @r2d@ with @realToFrac@ 
-- for @Float@ etc.
--
r2d :: Radian -> Double
r2d = (*) (180/pi) . fromRadian


-- | Modulo a (positive) angle into the range @0..2*pi@.
--
circularModulo :: Radian -> Radian
circularModulo r = d2r $ dec + (fromIntegral $ i `mod` 360)
  where
    i       :: Integer
    dec     :: Double
    (i,dec) = properFraction $ r2d r




--------------------------------------------------------------------------------
-- Bezier curves



kappa :: Floating u => u
kappa = 4 * ((sqrt 2 - 1) / 3)


-- | 'bezierCircle' : @ radius * center -> [Point] @ 
-- 
-- Make a circle from four Bezier curves. Although this function 
-- produces an approximation of a circle, the approximation seems
-- fine in practice.
--
bezierCircle :: (Fractional u, Floating u) 
              => u -> Point2 u -> [Point2 u]
bezierCircle radius (P2 x y) = 
    [ p00,c01,c02, p03,c04,c05, p06,c07,c08, p09,c10,c11, p00 ]
  where
    rl  = radius * kappa
    p00 = P2 (x + radius) y
    c01 = p00 .+^ vvec rl
    c02 = p03 .+^ hvec rl

    p03 = P2 x (y + radius) 
    c04 = p03 .+^ hvec (-rl)
    c05 = p06 .+^ vvec rl

    p06 = P2 (x - radius) y
    c07 = p06 .+^ vvec (-rl)
    c08 = p09 .+^ hvec (-rl)

    p09 = P2 x (y - radius) 
    c10 = p09 .+^ hvec rl
    c11 = p00 .+^ vvec (-rl)


-- | 'bezierEllipse' : @ x_radius * y_radius * center -> [Point] @ 
-- 
-- Make an ellipse from four Bezier curves. Although this function 
-- produces an approximation of a ellipse, the approximation seems
-- fine in practice.
--
bezierEllipse :: (Fractional u, Floating u) 
              => u -> u -> Point2 u -> [Point2 u]
bezierEllipse rx ry (P2 x y) = 
    [ p00,c01,c02, p03,c04,c05, p06,c07,c08, p09,c10,c11, p00 ]
  where
    lrx = rx * kappa
    lry = ry * kappa
    p00 = P2 (x + rx) y
    c01 = p00 .+^ vvec lry
    c02 = p03 .+^ hvec lrx

    p03 = P2 x (y + ry) 
    c04 = p03 .+^ hvec (-lrx)
    c05 = p06 .+^ vvec lry

    p06 = P2 (x - rx) y
    c07 = p06 .+^ vvec (-lry)
    c08 = p09 .+^ hvec (-lrx)

    p09 = P2 x (y - ry) 
    c10 = p09 .+^ hvec lrx
    c11 = p00 .+^ vvec (-lry)

-- | 'rbezierEllipse' : @ x_radius * y_radius * center * angle -> [Point] @ 
-- 
-- Make an rotated ellipse from four Bezier curves. 
-- 
-- Although this function produces an approximation of a ellipse, 
-- the approximation seems fine in practice.
--
rbezierEllipse :: (Real u, Floating u) 
               => u -> u -> Radian -> Point2 u -> [Point2 u]
rbezierEllipse rx ry theta pt@(P2 x y) = 
    [ p00,c01,c02, p03,c04,c05, p06,c07,c08, p09,c10,c11, p00 ]
  where
    lrx   = rx * kappa
    lry   = ry * kappa
    rotM  = originatedRotationMatrix theta pt

    --    hvec becomes para
    para  = \d -> avec theta d
    --    vvec becomes perp
    perp  = \d -> avec (circularModulo $ theta + pi*0.5) d
    mkPt  = \p1 -> rotM *# p1

    p00   = mkPt $ P2 (x + rx) y
    c01   = p00 .+^ perp lry
    c02   = p03 .+^ para lrx

    p03   = mkPt $ P2 x (y + ry) 
    c04   = p03 .+^ para (-lrx)
    c05   = p06 .+^ perp lry

    p06   = mkPt $ P2 (x - rx) y
    c07   = p06 .+^ perp (-lry)
    c08   = p09 .+^ para (-lrx)

    p09   = mkPt $ P2 x (y - ry) 
    c10   = p09 .+^ para lrx
    c11   = p00 .+^ perp (-lry)


-- | 'bezierArc' : @ radius * ang1 * ang2 * center -> 
--       (start_point, control_point1, control_point2, end_point) @
-- 
-- Create an arc - this construction is the analogue of 
-- PostScript\'s @arc@ command, but the arc is created as a 
-- Bezier curve so it should span less than 90deg.
--
-- CAVEAT - ang2 must be greater than ang1 
--
bezierArc :: Floating u 
          => u -> Radian -> Radian -> Point2 u 
          -> (Point2 u, Point2 u, Point2 u, Point2 u)
bezierArc r ang1 ang2 pt = (p0,p1,p2,p3)
  where
    theta = ang2 - ang1
    e     = r * fromRadian ((2 * sin (theta/2)) / (1+ 2 * cos (theta/2))) 
    p0    = pt .+^ avec ang1 r
    p1    = p0 .+^ avec (ang1 + pi/2) e
    p2    = p3 .+^ avec (ang2 - pi/2) e
    p3    = pt .+^ avec ang2 r


-- | 'subvisionCircle' : @ subdivisions * radius * center -> [Point] @ 
-- 
-- Make a circle from Bezier curves - the number of subdivsions 
-- controls the accuracy or the curve, more subdivisions produce
-- better curves, but less subdivisions are better for rendering
-- (producing more efficient PostScript).
--
-- Before revision 0.43.0, this was the only method in Wumpus to 
-- draw Bezier circles in Wumpus. However the kappa method seems
-- to draw equally good circles and is more efficient both in the
-- Haskell implementation and the generated PostScript code. This
-- function is retained for completeness and testing.
--
subdivisionCircle :: Floating u
                  => Int -> u -> Point2 u -> [Point2 u]
subdivisionCircle n radius pt = start $ subdivisions (n*4) (2*pi)
  where
    start (a:b:xs) = s : cp1 : cp2 : e : rest (b:xs)
      where (s,cp1,cp2,e) = bezierArc radius a b pt
                     
    start _        = [] 

    rest (a:b:xs)  = cp1 : cp2 : e : rest (b:xs)
      where (_,cp1,cp2,e) = bezierArc radius a b pt 

    rest _         = [] 

    subdivisions i a = 0 : take i (iterate (+one) one) 
      where  one  = a / fromIntegral i



