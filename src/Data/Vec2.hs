module Data.Vec2
  ( Vec2(..)
  , scalarMul
  , scalarDiv
  , dot
  , sqnorm
  , norm
  , normalize
  , proj
  , rotate
  , rotaten90
  , rotate90
  , arg
  , angleBetween
  , Vec2d
  )
where

import           Control.Applicative

import           CSB.Internal.Util

-- * Vector.

-- | 2-dimensional vector of any type.
data Vec2 a = Vec2 a a deriving (Eq, Show, Read, Functor)

instance Applicative Vec2 where
  pure x = Vec2 x x
  Vec2 fx fy <*> Vec2 x y = Vec2 (fx x) (fy y)

instance (Num a) => Num (Vec2 a) where
  (+)    = liftA2 (+)
  (*)    = liftA2 (*)
  abs    = fmap abs
  signum = fmap signum
  fromInteger n = pure (fromInteger n)
  negate = fmap negate

-- | Scalar multiplication.
scalarMul :: (Num a) => a -> Vec2 a -> Vec2 a
c `scalarMul` v = (* c) <$> v

-- | Scalar division.
scalarDiv :: (Fractional a) => Vec2 a -> a -> Vec2 a
v `scalarDiv` c = (/ c) <$> v

-- | Standard dot product.
dot :: (Num a) => Vec2 a -> Vec2 a -> a
Vec2 x1 y1 `dot` Vec2 x2 y2 = x1 * x2 + y1 * y2

-- | Square L2 norm.
sqnorm :: (Num a) => Vec2 a -> a
sqnorm v = dot v v

-- | L2 norm.
norm :: (Floating a) => Vec2 a -> a
norm = sqrt . sqnorm

-- | Normalize (i.e., scale to unit length) a vector.
normalize :: (Floating a) => Vec2 a -> Vec2 a
normalize v = v `scalarDiv` norm v

-- | Project a vector onto another.
proj v u = (u `dot` v / v `dot` v) `scalarMul` v

-- | Rotate a vector.
rotate :: (Floating a) => a -> Vec2 a -> Vec2 a
rotate theta (Vec2 x y) =
  Vec2 (x * cos theta - y * sin theta) (x * sin theta + y * cos theta)

-- | Rotate -90 degrees.
rotaten90 :: (Num a) => Vec2 a -> Vec2 a
rotaten90 (Vec2 x y) = Vec2 y (-x)

-- | Rotate 90 degrees.
rotate90 :: (Num a) => Vec2 a -> Vec2 a
rotate90 (Vec2 x y) = Vec2 (-y) x

-- | Argument.
arg :: (RealFloat a) => Vec2 a -> a
arg (Vec2 x y) = atan2 y x

-- | Angle between 2 vectors.
angleBetween :: (RealFloat a) => Vec2 a -> Vec2 a -> a
angleBetween from to = normalizeAngle $ arg to - arg from

-- * Convenient Aliases

-- | 2-dimensional vector of doubles.
type Vec2d = Vec2 Double
