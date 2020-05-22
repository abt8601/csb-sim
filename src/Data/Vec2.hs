module Data.Vec2
  ( Vec2(..)
  , Vec2i
  )
where

import           Control.Applicative

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

-- * Convenient Aliases

-- | 2-dimensional vector of integers.
type Vec2i = Vec2 Int
