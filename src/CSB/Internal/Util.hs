module CSB.Internal.Util
  ( clamp
  , fmod
  , normalizeAngle
  )
where

-- | Clamp a value between a minimum and a maximum.
clamp :: (Ord a) => a -> a -> a -> a
clamp mi ma = min ma . max mi

-- | Floating-point modulo.
fmod :: (RealFrac a) => a -> a -> a
x `fmod` y = x - y * fromIntegral (floor (x / y))

-- | Convert an angle to a value between [-pi, pi).
normalizeAngle :: (RealFrac a, Floating a) => a -> a
normalizeAngle = subtract pi . (`fmod` (2 * pi)) . (+ pi)
